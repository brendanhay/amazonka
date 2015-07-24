{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.DescribeWorkflowType
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified /workflow type/. This includes
-- configuration settings specified when the type was registered and other
-- information such as creation date, current status, etc.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
-- -   Constrain the following parameters by using a @Condition@ element
--     with the appropriate keys.
--     -   @workflowType.name@: String constraint. The key is
--         @swf:workflowType.name@.
--     -   @workflowType.version@: String constraint. The key is
--         @swf:workflowType.version@.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_DescribeWorkflowType.html>
module Network.AWS.SWF.DescribeWorkflowType
    (
    -- * Request
      DescribeWorkflowType
    -- ** Request constructor
    , describeWorkflowType
    -- ** Request lenses
    , dwtDomain
    , dwtWorkflowType

    -- * Response
    , DescribeWorkflowTypeResponse
    -- ** Response constructor
    , describeWorkflowTypeResponse
    -- ** Response lenses
    , dwtrsStatus
    , dwtrsTypeInfo
    , dwtrsConfiguration
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types

-- | /See:/ 'describeWorkflowType' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwtDomain'
--
-- * 'dwtWorkflowType'
data DescribeWorkflowType = DescribeWorkflowType'
    { _dwtDomain       :: !Text
    , _dwtWorkflowType :: !WorkflowType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeWorkflowType' smart constructor.
describeWorkflowType :: Text -> WorkflowType -> DescribeWorkflowType
describeWorkflowType pDomain_ pWorkflowType_ =
    DescribeWorkflowType'
    { _dwtDomain = pDomain_
    , _dwtWorkflowType = pWorkflowType_
    }

-- | The name of the domain in which this workflow type is registered.
dwtDomain :: Lens' DescribeWorkflowType Text
dwtDomain = lens _dwtDomain (\ s a -> s{_dwtDomain = a});

-- | The workflow type to describe.
dwtWorkflowType :: Lens' DescribeWorkflowType WorkflowType
dwtWorkflowType = lens _dwtWorkflowType (\ s a -> s{_dwtWorkflowType = a});

instance AWSRequest DescribeWorkflowType where
        type Sv DescribeWorkflowType = SWF
        type Rs DescribeWorkflowType =
             DescribeWorkflowTypeResponse
        request = postJSON "DescribeWorkflowType"
        response
          = receiveJSON
              (\ s h x ->
                 DescribeWorkflowTypeResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "typeInfo") <*>
                     (x .:> "configuration"))

instance ToHeaders DescribeWorkflowType where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.DescribeWorkflowType" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DescribeWorkflowType where
        toJSON DescribeWorkflowType'{..}
          = object
              ["domain" .= _dwtDomain,
               "workflowType" .= _dwtWorkflowType]

instance ToPath DescribeWorkflowType where
        toPath = const "/"

instance ToQuery DescribeWorkflowType where
        toQuery = const mempty

-- | Contains details about a workflow type.
--
-- /See:/ 'describeWorkflowTypeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwtrsStatus'
--
-- * 'dwtrsTypeInfo'
--
-- * 'dwtrsConfiguration'
data DescribeWorkflowTypeResponse = DescribeWorkflowTypeResponse'
    { _dwtrsStatus        :: !Int
    , _dwtrsTypeInfo      :: !WorkflowTypeInfo
    , _dwtrsConfiguration :: !WorkflowTypeConfiguration
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeWorkflowTypeResponse' smart constructor.
describeWorkflowTypeResponse :: Int -> WorkflowTypeInfo -> WorkflowTypeConfiguration -> DescribeWorkflowTypeResponse
describeWorkflowTypeResponse pStatus_ pTypeInfo_ pConfiguration_ =
    DescribeWorkflowTypeResponse'
    { _dwtrsStatus = pStatus_
    , _dwtrsTypeInfo = pTypeInfo_
    , _dwtrsConfiguration = pConfiguration_
    }

-- | FIXME: Undocumented member.
dwtrsStatus :: Lens' DescribeWorkflowTypeResponse Int
dwtrsStatus = lens _dwtrsStatus (\ s a -> s{_dwtrsStatus = a});

-- | General information about the workflow type.
--
-- The status of the workflow type (returned in the WorkflowTypeInfo
-- structure) can be one of the following.
--
-- -   __REGISTERED__: The type is registered and available. Workers
--     supporting this type should be running.
-- -   __DEPRECATED__: The type was deprecated using DeprecateWorkflowType,
--     but is still in use. You should keep workers supporting this type
--     running. You cannot create new workflow executions of this type.
dwtrsTypeInfo :: Lens' DescribeWorkflowTypeResponse WorkflowTypeInfo
dwtrsTypeInfo = lens _dwtrsTypeInfo (\ s a -> s{_dwtrsTypeInfo = a});

-- | Configuration settings of the workflow type registered through
-- RegisterWorkflowType
dwtrsConfiguration :: Lens' DescribeWorkflowTypeResponse WorkflowTypeConfiguration
dwtrsConfiguration = lens _dwtrsConfiguration (\ s a -> s{_dwtrsConfiguration = a});

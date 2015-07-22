{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.RegisterDomain
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Registers a new domain.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   You cannot use an IAM policy to control domain access for this
--     action. The name of the domain being registered is available as the
--     resource of this action.
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_RegisterDomain.html>
module Network.AWS.SWF.RegisterDomain
    (
    -- * Request
      RegisterDomain
    -- ** Request constructor
    , registerDomain
    -- ** Request lenses
    , rdrqDescription
    , rdrqName
    , rdrqWorkflowExecutionRetentionPeriodInDays

    -- * Response
    , RegisterDomainResponse
    -- ** Response constructor
    , registerDomainResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types

-- | /See:/ 'registerDomain' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdrqDescription'
--
-- * 'rdrqName'
--
-- * 'rdrqWorkflowExecutionRetentionPeriodInDays'
data RegisterDomain = RegisterDomain'
    { _rdrqDescription                            :: !(Maybe Text)
    , _rdrqName                                   :: !Text
    , _rdrqWorkflowExecutionRetentionPeriodInDays :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterDomain' smart constructor.
registerDomain :: Text -> Text -> RegisterDomain
registerDomain pName pWorkflowExecutionRetentionPeriodInDays =
    RegisterDomain'
    { _rdrqDescription = Nothing
    , _rdrqName = pName
    , _rdrqWorkflowExecutionRetentionPeriodInDays = pWorkflowExecutionRetentionPeriodInDays
    }

-- | A text description of the domain.
rdrqDescription :: Lens' RegisterDomain (Maybe Text)
rdrqDescription = lens _rdrqDescription (\ s a -> s{_rdrqDescription = a});

-- | Name of the domain to register. The name must be unique in the region
-- that the domain is registered in.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (\\u0000-\\u001f | \\u007f - \\u009f). Also, it must not
-- contain the literal string quotarnquot.
rdrqName :: Lens' RegisterDomain Text
rdrqName = lens _rdrqName (\ s a -> s{_rdrqName = a});

-- | The duration (in days) that records and histories of workflow executions
-- on the domain should be kept by the service. After the retention period,
-- the workflow execution is not available in the results of visibility
-- calls.
--
-- If you pass the value @NONE@ or @0@ (zero), then the workflow execution
-- history will not be retained. As soon as the workflow execution
-- completes, the execution record and its history are deleted.
--
-- The maximum workflow execution retention period is 90 days. For more
-- information about Amazon SWF service limits, see:
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dg-limits.html Amazon SWF Service Limits>
-- in the /Amazon SWF Developer Guide/.
rdrqWorkflowExecutionRetentionPeriodInDays :: Lens' RegisterDomain Text
rdrqWorkflowExecutionRetentionPeriodInDays = lens _rdrqWorkflowExecutionRetentionPeriodInDays (\ s a -> s{_rdrqWorkflowExecutionRetentionPeriodInDays = a});

instance AWSRequest RegisterDomain where
        type Sv RegisterDomain = SWF
        type Rs RegisterDomain = RegisterDomainResponse
        request = postJSON
        response = receiveNull RegisterDomainResponse'

instance ToHeaders RegisterDomain where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.RegisterDomain" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON RegisterDomain where
        toJSON RegisterDomain'{..}
          = object
              ["description" .= _rdrqDescription,
               "name" .= _rdrqName,
               "workflowExecutionRetentionPeriodInDays" .=
                 _rdrqWorkflowExecutionRetentionPeriodInDays]

instance ToPath RegisterDomain where
        toPath = const "/"

instance ToQuery RegisterDomain where
        toQuery = const mempty

-- | /See:/ 'registerDomainResponse' smart constructor.
data RegisterDomainResponse =
    RegisterDomainResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterDomainResponse' smart constructor.
registerDomainResponse :: RegisterDomainResponse
registerDomainResponse = RegisterDomainResponse'

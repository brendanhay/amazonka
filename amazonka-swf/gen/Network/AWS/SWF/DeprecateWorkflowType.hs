{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.DeprecateWorkflowType
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecates the specified /workflow type/. After a workflow type has been
-- deprecated, you cannot create new executions of that type. Executions
-- that were started before the type was deprecated will continue to run. A
-- deprecated workflow type may still be used when calling visibility
-- actions.
--
-- This operation is eventually consistent. The results are best effort and
-- may not exactly reflect recent updates and changes.
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
-- /See:/ <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_DeprecateWorkflowType.html AWS API Reference> for DeprecateWorkflowType.
module Network.AWS.SWF.DeprecateWorkflowType
    (
    -- * Creating a Request
      DeprecateWorkflowType
    , deprecateWorkflowType
    -- * Request Lenses
    , dDomain
    , dWorkflowType

    -- * Destructuring the Response
    , DeprecateWorkflowTypeResponse
    , deprecateWorkflowTypeResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types
import           Network.AWS.SWF.Types.Product

-- | /See:/ 'deprecateWorkflowType' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dDomain'
--
-- * 'dWorkflowType'
data DeprecateWorkflowType = DeprecateWorkflowType'
    { _dDomain       :: !Text
    , _dWorkflowType :: !WorkflowType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeprecateWorkflowType' smart constructor.
deprecateWorkflowType :: Text -> WorkflowType -> DeprecateWorkflowType
deprecateWorkflowType pDomain_ pWorkflowType_ =
    DeprecateWorkflowType'
    { _dDomain = pDomain_
    , _dWorkflowType = pWorkflowType_
    }

-- | The name of the domain in which the workflow type is registered.
dDomain :: Lens' DeprecateWorkflowType Text
dDomain = lens _dDomain (\ s a -> s{_dDomain = a});

-- | The workflow type to deprecate.
dWorkflowType :: Lens' DeprecateWorkflowType WorkflowType
dWorkflowType = lens _dWorkflowType (\ s a -> s{_dWorkflowType = a});

instance AWSRequest DeprecateWorkflowType where
        type Sv DeprecateWorkflowType = SWF
        type Rs DeprecateWorkflowType =
             DeprecateWorkflowTypeResponse
        request = postJSON
        response = receiveNull DeprecateWorkflowTypeResponse'

instance ToHeaders DeprecateWorkflowType where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.DeprecateWorkflowType" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DeprecateWorkflowType where
        toJSON DeprecateWorkflowType'{..}
          = object
              ["domain" .= _dDomain,
               "workflowType" .= _dWorkflowType]

instance ToPath DeprecateWorkflowType where
        toPath = const "/"

instance ToQuery DeprecateWorkflowType where
        toQuery = const mempty

-- | /See:/ 'deprecateWorkflowTypeResponse' smart constructor.
data DeprecateWorkflowTypeResponse =
    DeprecateWorkflowTypeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeprecateWorkflowTypeResponse' smart constructor.
deprecateWorkflowTypeResponse :: DeprecateWorkflowTypeResponse
deprecateWorkflowTypeResponse = DeprecateWorkflowTypeResponse'

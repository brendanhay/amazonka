{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.SWF.DeprecateWorkflowType
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deprecates the specified /workflow type/. After a workflow type has been
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
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_DeprecateWorkflowType.html>
module Network.AWS.SWF.DeprecateWorkflowType
    (
    -- * Request
      DeprecateWorkflowType
    -- ** Request constructor
    , deprecateWorkflowType
    -- ** Request lenses
    , depDomain
    , depWorkflowType

    -- * Response
    , DeprecateWorkflowTypeResponse
    -- ** Response constructor
    , deprecateWorkflowTypeResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types

-- | /See:/ 'deprecateWorkflowType' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'depDomain'
--
-- * 'depWorkflowType'
data DeprecateWorkflowType = DeprecateWorkflowType'
    { _depDomain       :: !Text
    , _depWorkflowType :: !WorkflowType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeprecateWorkflowType' smart constructor.
deprecateWorkflowType :: Text -> WorkflowType -> DeprecateWorkflowType
deprecateWorkflowType pDomain pWorkflowType =
    DeprecateWorkflowType'
    { _depDomain = pDomain
    , _depWorkflowType = pWorkflowType
    }

-- | The name of the domain in which the workflow type is registered.
depDomain :: Lens' DeprecateWorkflowType Text
depDomain = lens _depDomain (\ s a -> s{_depDomain = a});

-- | The workflow type to deprecate.
depWorkflowType :: Lens' DeprecateWorkflowType WorkflowType
depWorkflowType = lens _depWorkflowType (\ s a -> s{_depWorkflowType = a});

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
              ["domain" .= _depDomain,
               "workflowType" .= _depWorkflowType]

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

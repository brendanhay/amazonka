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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecates the specified /workflow type/ . After a workflow type has been deprecated, you cannot create new executions of that type. Executions that were started before the type was deprecated continues to run. A deprecated workflow type may still be used when calling visibility actions.
--
--
-- __Access Control__
--
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--     * Constrain the following parameters by using a @Condition@ element with the appropriate keys.
--
--     * @workflowType.name@ : String constraint. The key is @swf:workflowType.name@ .
--
--     * @workflowType.version@ : String constraint. The key is @swf:workflowType.version@ .
--
--
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
module Network.AWS.SWF.DeprecateWorkflowType
    (
    -- * Creating a Request
      deprecateWorkflowType
    , DeprecateWorkflowType
    -- * Request Lenses
    , dDomain
    , dWorkflowType

    -- * Destructuring the Response
    , deprecateWorkflowTypeResponse
    , DeprecateWorkflowTypeResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types
import Network.AWS.SWF.Types.Product

-- | /See:/ 'deprecateWorkflowType' smart constructor.
data DeprecateWorkflowType = DeprecateWorkflowType'
  { _dDomain       :: !Text
  , _dWorkflowType :: !WorkflowType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeprecateWorkflowType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDomain' - The name of the domain in which the workflow type is registered.
--
-- * 'dWorkflowType' - The workflow type to deprecate.
deprecateWorkflowType
    :: Text -- ^ 'dDomain'
    -> WorkflowType -- ^ 'dWorkflowType'
    -> DeprecateWorkflowType
deprecateWorkflowType pDomain_ pWorkflowType_ =
  DeprecateWorkflowType' {_dDomain = pDomain_, _dWorkflowType = pWorkflowType_}


-- | The name of the domain in which the workflow type is registered.
dDomain :: Lens' DeprecateWorkflowType Text
dDomain = lens _dDomain (\ s a -> s{_dDomain = a})

-- | The workflow type to deprecate.
dWorkflowType :: Lens' DeprecateWorkflowType WorkflowType
dWorkflowType = lens _dWorkflowType (\ s a -> s{_dWorkflowType = a})

instance AWSRequest DeprecateWorkflowType where
        type Rs DeprecateWorkflowType =
             DeprecateWorkflowTypeResponse
        request = postJSON swf
        response = receiveNull DeprecateWorkflowTypeResponse'

instance Hashable DeprecateWorkflowType where

instance NFData DeprecateWorkflowType where

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
              (catMaybes
                 [Just ("domain" .= _dDomain),
                  Just ("workflowType" .= _dWorkflowType)])

instance ToPath DeprecateWorkflowType where
        toPath = const "/"

instance ToQuery DeprecateWorkflowType where
        toQuery = const mempty

-- | /See:/ 'deprecateWorkflowTypeResponse' smart constructor.
data DeprecateWorkflowTypeResponse =
  DeprecateWorkflowTypeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeprecateWorkflowTypeResponse' with the minimum fields required to make a request.
--
deprecateWorkflowTypeResponse
    :: DeprecateWorkflowTypeResponse
deprecateWorkflowTypeResponse = DeprecateWorkflowTypeResponse'


instance NFData DeprecateWorkflowTypeResponse where

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.UndeprecateWorkflowType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undeprecates a previously deprecated /workflow type/ . After a workflow type has been undeprecated, you can create new executions of that type.
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
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
module Network.AWS.SWF.UndeprecateWorkflowType
  ( -- * Creating a Request
    undeprecateWorkflowType,
    UndeprecateWorkflowType,

    -- * Request Lenses
    uwtDomain,
    uwtWorkflowType,

    -- * Destructuring the Response
    undeprecateWorkflowTypeResponse,
    UndeprecateWorkflowTypeResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types

-- | /See:/ 'undeprecateWorkflowType' smart constructor.
data UndeprecateWorkflowType = UndeprecateWorkflowType'
  { _uwtDomain ::
      !Text,
    _uwtWorkflowType :: !WorkflowType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UndeprecateWorkflowType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uwtDomain' - The name of the domain of the deprecated workflow type.
--
-- * 'uwtWorkflowType' - The name of the domain of the deprecated workflow type.
undeprecateWorkflowType ::
  -- | 'uwtDomain'
  Text ->
  -- | 'uwtWorkflowType'
  WorkflowType ->
  UndeprecateWorkflowType
undeprecateWorkflowType pDomain_ pWorkflowType_ =
  UndeprecateWorkflowType'
    { _uwtDomain = pDomain_,
      _uwtWorkflowType = pWorkflowType_
    }

-- | The name of the domain of the deprecated workflow type.
uwtDomain :: Lens' UndeprecateWorkflowType Text
uwtDomain = lens _uwtDomain (\s a -> s {_uwtDomain = a})

-- | The name of the domain of the deprecated workflow type.
uwtWorkflowType :: Lens' UndeprecateWorkflowType WorkflowType
uwtWorkflowType = lens _uwtWorkflowType (\s a -> s {_uwtWorkflowType = a})

instance AWSRequest UndeprecateWorkflowType where
  type Rs UndeprecateWorkflowType = UndeprecateWorkflowTypeResponse
  request = postJSON swf
  response = receiveNull UndeprecateWorkflowTypeResponse'

instance Hashable UndeprecateWorkflowType

instance NFData UndeprecateWorkflowType

instance ToHeaders UndeprecateWorkflowType where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SimpleWorkflowService.UndeprecateWorkflowType" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON UndeprecateWorkflowType where
  toJSON UndeprecateWorkflowType' {..} =
    object
      ( catMaybes
          [ Just ("domain" .= _uwtDomain),
            Just ("workflowType" .= _uwtWorkflowType)
          ]
      )

instance ToPath UndeprecateWorkflowType where
  toPath = const "/"

instance ToQuery UndeprecateWorkflowType where
  toQuery = const mempty

-- | /See:/ 'undeprecateWorkflowTypeResponse' smart constructor.
data UndeprecateWorkflowTypeResponse = UndeprecateWorkflowTypeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UndeprecateWorkflowTypeResponse' with the minimum fields required to make a request.
undeprecateWorkflowTypeResponse ::
  UndeprecateWorkflowTypeResponse
undeprecateWorkflowTypeResponse = UndeprecateWorkflowTypeResponse'

instance NFData UndeprecateWorkflowTypeResponse

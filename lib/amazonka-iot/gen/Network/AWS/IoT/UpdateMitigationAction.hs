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
-- Module      : Network.AWS.IoT.UpdateMitigationAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the definition for the specified mitigation action.
module Network.AWS.IoT.UpdateMitigationAction
  ( -- * Creating a Request
    updateMitigationAction,
    UpdateMitigationAction,

    -- * Request Lenses
    umaActionParams,
    umaRoleARN,
    umaActionName,

    -- * Destructuring the Response
    updateMitigationActionResponse,
    UpdateMitigationActionResponse,

    -- * Response Lenses
    umarsActionId,
    umarsActionARN,
    umarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateMitigationAction' smart constructor.
data UpdateMitigationAction = UpdateMitigationAction'
  { _umaActionParams ::
      !(Maybe MitigationActionParams),
    _umaRoleARN :: !(Maybe Text),
    _umaActionName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateMitigationAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umaActionParams' - Defines the type of action and the parameters for that action.
--
-- * 'umaRoleARN' - The ARN of the IAM role that is used to apply the mitigation action.
--
-- * 'umaActionName' - The friendly name for the mitigation action. You can't change the name by using @UpdateMitigationAction@ . Instead, you must delete and re-create the mitigation action with the new name.
updateMitigationAction ::
  -- | 'umaActionName'
  Text ->
  UpdateMitigationAction
updateMitigationAction pActionName_ =
  UpdateMitigationAction'
    { _umaActionParams = Nothing,
      _umaRoleARN = Nothing,
      _umaActionName = pActionName_
    }

-- | Defines the type of action and the parameters for that action.
umaActionParams :: Lens' UpdateMitigationAction (Maybe MitigationActionParams)
umaActionParams = lens _umaActionParams (\s a -> s {_umaActionParams = a})

-- | The ARN of the IAM role that is used to apply the mitigation action.
umaRoleARN :: Lens' UpdateMitigationAction (Maybe Text)
umaRoleARN = lens _umaRoleARN (\s a -> s {_umaRoleARN = a})

-- | The friendly name for the mitigation action. You can't change the name by using @UpdateMitigationAction@ . Instead, you must delete and re-create the mitigation action with the new name.
umaActionName :: Lens' UpdateMitigationAction Text
umaActionName = lens _umaActionName (\s a -> s {_umaActionName = a})

instance AWSRequest UpdateMitigationAction where
  type Rs UpdateMitigationAction = UpdateMitigationActionResponse
  request = patchJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          UpdateMitigationActionResponse'
            <$> (x .?> "actionId") <*> (x .?> "actionArn") <*> (pure (fromEnum s))
      )

instance Hashable UpdateMitigationAction

instance NFData UpdateMitigationAction

instance ToHeaders UpdateMitigationAction where
  toHeaders = const mempty

instance ToJSON UpdateMitigationAction where
  toJSON UpdateMitigationAction' {..} =
    object
      ( catMaybes
          [ ("actionParams" .=) <$> _umaActionParams,
            ("roleArn" .=) <$> _umaRoleARN
          ]
      )

instance ToPath UpdateMitigationAction where
  toPath UpdateMitigationAction' {..} =
    mconcat ["/mitigationactions/actions/", toBS _umaActionName]

instance ToQuery UpdateMitigationAction where
  toQuery = const mempty

-- | /See:/ 'updateMitigationActionResponse' smart constructor.
data UpdateMitigationActionResponse = UpdateMitigationActionResponse'
  { _umarsActionId ::
      !(Maybe Text),
    _umarsActionARN ::
      !(Maybe Text),
    _umarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateMitigationActionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umarsActionId' - A unique identifier for the mitigation action.
--
-- * 'umarsActionARN' - The ARN for the new mitigation action.
--
-- * 'umarsResponseStatus' - -- | The response status code.
updateMitigationActionResponse ::
  -- | 'umarsResponseStatus'
  Int ->
  UpdateMitigationActionResponse
updateMitigationActionResponse pResponseStatus_ =
  UpdateMitigationActionResponse'
    { _umarsActionId = Nothing,
      _umarsActionARN = Nothing,
      _umarsResponseStatus = pResponseStatus_
    }

-- | A unique identifier for the mitigation action.
umarsActionId :: Lens' UpdateMitigationActionResponse (Maybe Text)
umarsActionId = lens _umarsActionId (\s a -> s {_umarsActionId = a})

-- | The ARN for the new mitigation action.
umarsActionARN :: Lens' UpdateMitigationActionResponse (Maybe Text)
umarsActionARN = lens _umarsActionARN (\s a -> s {_umarsActionARN = a})

-- | -- | The response status code.
umarsResponseStatus :: Lens' UpdateMitigationActionResponse Int
umarsResponseStatus = lens _umarsResponseStatus (\s a -> s {_umarsResponseStatus = a})

instance NFData UpdateMitigationActionResponse

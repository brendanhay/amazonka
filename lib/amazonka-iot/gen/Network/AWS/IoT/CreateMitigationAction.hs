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
-- Module      : Network.AWS.IoT.CreateMitigationAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Defines an action that can be applied to audit findings by using StartAuditMitigationActionsTask. Only certain types of mitigation actions can be applied to specific check names. For more information, see <https://docs.aws.amazon.com/iot/latest/developerguide/device-defender-mitigation-actions.html Mitigation actions> . Each mitigation action can apply only one type of change.
module Network.AWS.IoT.CreateMitigationAction
  ( -- * Creating a Request
    createMitigationAction,
    CreateMitigationAction,

    -- * Request Lenses
    cTags,
    cActionName,
    cRoleARN,
    cActionParams,

    -- * Destructuring the Response
    createMitigationActionResponse,
    CreateMitigationActionResponse,

    -- * Response Lenses
    cmarsActionId,
    cmarsActionARN,
    cmarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createMitigationAction' smart constructor.
data CreateMitigationAction = CreateMitigationAction'
  { _cTags ::
      !(Maybe [Tag]),
    _cActionName :: !Text,
    _cRoleARN :: !Text,
    _cActionParams :: !MitigationActionParams
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateMitigationAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cTags' - Metadata that can be used to manage the mitigation action.
--
-- * 'cActionName' - A friendly name for the action. Choose a friendly name that accurately describes the action (for example, @EnableLoggingAction@ ).
--
-- * 'cRoleARN' - The ARN of the IAM role that is used to apply the mitigation action.
--
-- * 'cActionParams' - Defines the type of action and the parameters for that action.
createMitigationAction ::
  -- | 'cActionName'
  Text ->
  -- | 'cRoleARN'
  Text ->
  -- | 'cActionParams'
  MitigationActionParams ->
  CreateMitigationAction
createMitigationAction pActionName_ pRoleARN_ pActionParams_ =
  CreateMitigationAction'
    { _cTags = Nothing,
      _cActionName = pActionName_,
      _cRoleARN = pRoleARN_,
      _cActionParams = pActionParams_
    }

-- | Metadata that can be used to manage the mitigation action.
cTags :: Lens' CreateMitigationAction [Tag]
cTags = lens _cTags (\s a -> s {_cTags = a}) . _Default . _Coerce

-- | A friendly name for the action. Choose a friendly name that accurately describes the action (for example, @EnableLoggingAction@ ).
cActionName :: Lens' CreateMitigationAction Text
cActionName = lens _cActionName (\s a -> s {_cActionName = a})

-- | The ARN of the IAM role that is used to apply the mitigation action.
cRoleARN :: Lens' CreateMitigationAction Text
cRoleARN = lens _cRoleARN (\s a -> s {_cRoleARN = a})

-- | Defines the type of action and the parameters for that action.
cActionParams :: Lens' CreateMitigationAction MitigationActionParams
cActionParams = lens _cActionParams (\s a -> s {_cActionParams = a})

instance AWSRequest CreateMitigationAction where
  type Rs CreateMitigationAction = CreateMitigationActionResponse
  request = postJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          CreateMitigationActionResponse'
            <$> (x .?> "actionId") <*> (x .?> "actionArn") <*> (pure (fromEnum s))
      )

instance Hashable CreateMitigationAction

instance NFData CreateMitigationAction

instance ToHeaders CreateMitigationAction where
  toHeaders = const mempty

instance ToJSON CreateMitigationAction where
  toJSON CreateMitigationAction' {..} =
    object
      ( catMaybes
          [ ("tags" .=) <$> _cTags,
            Just ("roleArn" .= _cRoleARN),
            Just ("actionParams" .= _cActionParams)
          ]
      )

instance ToPath CreateMitigationAction where
  toPath CreateMitigationAction' {..} =
    mconcat ["/mitigationactions/actions/", toBS _cActionName]

instance ToQuery CreateMitigationAction where
  toQuery = const mempty

-- | /See:/ 'createMitigationActionResponse' smart constructor.
data CreateMitigationActionResponse = CreateMitigationActionResponse'
  { _cmarsActionId ::
      !(Maybe Text),
    _cmarsActionARN ::
      !(Maybe Text),
    _cmarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateMitigationActionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmarsActionId' - A unique identifier for the new mitigation action.
--
-- * 'cmarsActionARN' - The ARN for the new mitigation action.
--
-- * 'cmarsResponseStatus' - -- | The response status code.
createMitigationActionResponse ::
  -- | 'cmarsResponseStatus'
  Int ->
  CreateMitigationActionResponse
createMitigationActionResponse pResponseStatus_ =
  CreateMitigationActionResponse'
    { _cmarsActionId = Nothing,
      _cmarsActionARN = Nothing,
      _cmarsResponseStatus = pResponseStatus_
    }

-- | A unique identifier for the new mitigation action.
cmarsActionId :: Lens' CreateMitigationActionResponse (Maybe Text)
cmarsActionId = lens _cmarsActionId (\s a -> s {_cmarsActionId = a})

-- | The ARN for the new mitigation action.
cmarsActionARN :: Lens' CreateMitigationActionResponse (Maybe Text)
cmarsActionARN = lens _cmarsActionARN (\s a -> s {_cmarsActionARN = a})

-- | -- | The response status code.
cmarsResponseStatus :: Lens' CreateMitigationActionResponse Int
cmarsResponseStatus = lens _cmarsResponseStatus (\s a -> s {_cmarsResponseStatus = a})

instance NFData CreateMitigationActionResponse

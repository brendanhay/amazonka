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
-- Module      : Network.AWS.SecretsManager.UpdateSecretVersionStage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the staging labels attached to a version of a secret. Staging labels are used to track a version as it progresses through the secret rotation process. You can attach a staging label to only one version of a secret at a time. If a staging label to be added is already attached to another version, then it is moved--removed from the other version first and then attached to this one. For more information about staging labels, see <https://docs.aws.amazon.com/secretsmanager/latest/userguide/terms-concepts.html#term_staging-label Staging Labels> in the /AWS Secrets Manager User Guide/ .
--
--
-- The staging labels that you specify in the @VersionStage@ parameter are added to the existing list of staging labels--they don't replace it.
--
-- You can move the @AWSCURRENT@ staging label to this version by including it in this call.
--
-- If this action results in the last label being removed from a version, then the version is considered to be 'deprecated' and can be deleted by Secrets Manager.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:UpdateSecretVersionStage
--
--
--
-- __Related operations__
--
--     * To get the list of staging labels that are currently associated with a version of a secret, use @'DescribeSecret' @ and examine the @SecretVersionsToStages@ response value.
module Network.AWS.SecretsManager.UpdateSecretVersionStage
  ( -- * Creating a Request
    updateSecretVersionStage,
    UpdateSecretVersionStage,

    -- * Request Lenses
    usvsRemoveFromVersionId,
    usvsMoveToVersionId,
    usvsSecretId,
    usvsVersionStage,

    -- * Destructuring the Response
    updateSecretVersionStageResponse,
    UpdateSecretVersionStageResponse,

    -- * Response Lenses
    usvsrsARN,
    usvsrsName,
    usvsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SecretsManager.Types

-- | /See:/ 'updateSecretVersionStage' smart constructor.
data UpdateSecretVersionStage = UpdateSecretVersionStage'
  { _usvsRemoveFromVersionId ::
      !(Maybe Text),
    _usvsMoveToVersionId :: !(Maybe Text),
    _usvsSecretId :: !Text,
    _usvsVersionStage :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateSecretVersionStage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usvsRemoveFromVersionId' - Specifies the secret version ID of the version that the staging label is to be removed from. If the staging label you are trying to attach to one version is already attached to a different version, then you must include this parameter and specify the version that the label is to be removed from. If the label is attached and you either do not specify this parameter, or the version ID does not match, then the operation fails.
--
-- * 'usvsMoveToVersionId' - (Optional) The secret version ID that you want to add the staging label. If you want to remove a label from a version, then do not specify this parameter. If the staging label is already attached to a different version of the secret, then you must also specify the @RemoveFromVersionId@ parameter.
--
-- * 'usvsSecretId' - Specifies the secret with the version with the list of staging labels you want to modify. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- * 'usvsVersionStage' - The staging label to add to this version.
updateSecretVersionStage ::
  -- | 'usvsSecretId'
  Text ->
  -- | 'usvsVersionStage'
  Text ->
  UpdateSecretVersionStage
updateSecretVersionStage pSecretId_ pVersionStage_ =
  UpdateSecretVersionStage'
    { _usvsRemoveFromVersionId = Nothing,
      _usvsMoveToVersionId = Nothing,
      _usvsSecretId = pSecretId_,
      _usvsVersionStage = pVersionStage_
    }

-- | Specifies the secret version ID of the version that the staging label is to be removed from. If the staging label you are trying to attach to one version is already attached to a different version, then you must include this parameter and specify the version that the label is to be removed from. If the label is attached and you either do not specify this parameter, or the version ID does not match, then the operation fails.
usvsRemoveFromVersionId :: Lens' UpdateSecretVersionStage (Maybe Text)
usvsRemoveFromVersionId = lens _usvsRemoveFromVersionId (\s a -> s {_usvsRemoveFromVersionId = a})

-- | (Optional) The secret version ID that you want to add the staging label. If you want to remove a label from a version, then do not specify this parameter. If the staging label is already attached to a different version of the secret, then you must also specify the @RemoveFromVersionId@ parameter.
usvsMoveToVersionId :: Lens' UpdateSecretVersionStage (Maybe Text)
usvsMoveToVersionId = lens _usvsMoveToVersionId (\s a -> s {_usvsMoveToVersionId = a})

-- | Specifies the secret with the version with the list of staging labels you want to modify. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
usvsSecretId :: Lens' UpdateSecretVersionStage Text
usvsSecretId = lens _usvsSecretId (\s a -> s {_usvsSecretId = a})

-- | The staging label to add to this version.
usvsVersionStage :: Lens' UpdateSecretVersionStage Text
usvsVersionStage = lens _usvsVersionStage (\s a -> s {_usvsVersionStage = a})

instance AWSRequest UpdateSecretVersionStage where
  type Rs UpdateSecretVersionStage = UpdateSecretVersionStageResponse
  request = postJSON secretsManager
  response =
    receiveJSON
      ( \s h x ->
          UpdateSecretVersionStageResponse'
            <$> (x .?> "ARN") <*> (x .?> "Name") <*> (pure (fromEnum s))
      )

instance Hashable UpdateSecretVersionStage

instance NFData UpdateSecretVersionStage

instance ToHeaders UpdateSecretVersionStage where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("secretsmanager.UpdateSecretVersionStage" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateSecretVersionStage where
  toJSON UpdateSecretVersionStage' {..} =
    object
      ( catMaybes
          [ ("RemoveFromVersionId" .=) <$> _usvsRemoveFromVersionId,
            ("MoveToVersionId" .=) <$> _usvsMoveToVersionId,
            Just ("SecretId" .= _usvsSecretId),
            Just ("VersionStage" .= _usvsVersionStage)
          ]
      )

instance ToPath UpdateSecretVersionStage where
  toPath = const "/"

instance ToQuery UpdateSecretVersionStage where
  toQuery = const mempty

-- | /See:/ 'updateSecretVersionStageResponse' smart constructor.
data UpdateSecretVersionStageResponse = UpdateSecretVersionStageResponse'
  { _usvsrsARN ::
      !(Maybe Text),
    _usvsrsName ::
      !(Maybe Text),
    _usvsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateSecretVersionStageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usvsrsARN' - The ARN of the secret with the modified staging label.
--
-- * 'usvsrsName' - The friendly name of the secret with the modified staging label.
--
-- * 'usvsrsResponseStatus' - -- | The response status code.
updateSecretVersionStageResponse ::
  -- | 'usvsrsResponseStatus'
  Int ->
  UpdateSecretVersionStageResponse
updateSecretVersionStageResponse pResponseStatus_ =
  UpdateSecretVersionStageResponse'
    { _usvsrsARN = Nothing,
      _usvsrsName = Nothing,
      _usvsrsResponseStatus = pResponseStatus_
    }

-- | The ARN of the secret with the modified staging label.
usvsrsARN :: Lens' UpdateSecretVersionStageResponse (Maybe Text)
usvsrsARN = lens _usvsrsARN (\s a -> s {_usvsrsARN = a})

-- | The friendly name of the secret with the modified staging label.
usvsrsName :: Lens' UpdateSecretVersionStageResponse (Maybe Text)
usvsrsName = lens _usvsrsName (\s a -> s {_usvsrsName = a})

-- | -- | The response status code.
usvsrsResponseStatus :: Lens' UpdateSecretVersionStageResponse Int
usvsrsResponseStatus = lens _usvsrsResponseStatus (\s a -> s {_usvsrsResponseStatus = a})

instance NFData UpdateSecretVersionStageResponse

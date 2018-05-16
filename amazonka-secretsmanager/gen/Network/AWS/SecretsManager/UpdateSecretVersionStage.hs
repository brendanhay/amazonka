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
-- Module      : Network.AWS.SecretsManager.UpdateSecretVersionStage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the staging labels attached to a version of a secret. Staging labels are used to track a version as it progresses through the secret rotation process. You can attach a staging label to only one version of a secret at a time. If a staging label to be added is already attached to another version, then it is moved--removed from the other version first and then attached to this one. For more information about staging labels, see <http://docs.aws.amazon.com/secretsmanager/latest/userguide/terms-concepts.html#term_staging-label Staging Labels> in the /AWS Secrets Manager User Guide/ .
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
--
--
--
module Network.AWS.SecretsManager.UpdateSecretVersionStage
    (
    -- * Creating a Request
      updateSecretVersionStage
    , UpdateSecretVersionStage
    -- * Request Lenses
    , usvsRemoveFromVersionId
    , usvsMoveToVersionId
    , usvsSecretId
    , usvsVersionStage

    -- * Destructuring the Response
    , updateSecretVersionStageResponse
    , UpdateSecretVersionStageResponse
    -- * Response Lenses
    , usvsrsARN
    , usvsrsName
    , usvsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SecretsManager.Types
import Network.AWS.SecretsManager.Types.Product

-- | /See:/ 'updateSecretVersionStage' smart constructor.
data UpdateSecretVersionStage = UpdateSecretVersionStage'
  { _usvsRemoveFromVersionId :: !(Maybe Text)
  , _usvsMoveToVersionId     :: !(Maybe Text)
  , _usvsSecretId            :: !Text
  , _usvsVersionStage        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSecretVersionStage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usvsRemoveFromVersionId' - (Optional) Specifies the secret version ID of the version that the staging labels are to be removed from. If you want to move a label to a new version, you do not have to explicitly remove it with this parameter. Adding a label using the @MoveToVersionId@ parameter automatically removes it from the old version. However, if you do include both the "MoveTo" and "RemoveFrom" parameters, then the move is successful only if the staging labels are actually present on the "RemoveFrom" version. If a staging label was on a different version than "RemoveFrom", then the request fails.
--
-- * 'usvsMoveToVersionId' - (Optional) The secret version ID that you want to add the staging labels to. If any of the staging labels are already attached to a different version of the secret, then they are removed from that version before adding them to this version.
--
-- * 'usvsSecretId' - Specifies the secret with the version whose list of staging labels you want to modify. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- * 'usvsVersionStage' - The list of staging labels to add to this version.
updateSecretVersionStage
    :: Text -- ^ 'usvsSecretId'
    -> Text -- ^ 'usvsVersionStage'
    -> UpdateSecretVersionStage
updateSecretVersionStage pSecretId_ pVersionStage_ =
  UpdateSecretVersionStage'
    { _usvsRemoveFromVersionId = Nothing
    , _usvsMoveToVersionId = Nothing
    , _usvsSecretId = pSecretId_
    , _usvsVersionStage = pVersionStage_
    }


-- | (Optional) Specifies the secret version ID of the version that the staging labels are to be removed from. If you want to move a label to a new version, you do not have to explicitly remove it with this parameter. Adding a label using the @MoveToVersionId@ parameter automatically removes it from the old version. However, if you do include both the "MoveTo" and "RemoveFrom" parameters, then the move is successful only if the staging labels are actually present on the "RemoveFrom" version. If a staging label was on a different version than "RemoveFrom", then the request fails.
usvsRemoveFromVersionId :: Lens' UpdateSecretVersionStage (Maybe Text)
usvsRemoveFromVersionId = lens _usvsRemoveFromVersionId (\ s a -> s{_usvsRemoveFromVersionId = a})

-- | (Optional) The secret version ID that you want to add the staging labels to. If any of the staging labels are already attached to a different version of the secret, then they are removed from that version before adding them to this version.
usvsMoveToVersionId :: Lens' UpdateSecretVersionStage (Maybe Text)
usvsMoveToVersionId = lens _usvsMoveToVersionId (\ s a -> s{_usvsMoveToVersionId = a})

-- | Specifies the secret with the version whose list of staging labels you want to modify. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
usvsSecretId :: Lens' UpdateSecretVersionStage Text
usvsSecretId = lens _usvsSecretId (\ s a -> s{_usvsSecretId = a})

-- | The list of staging labels to add to this version.
usvsVersionStage :: Lens' UpdateSecretVersionStage Text
usvsVersionStage = lens _usvsVersionStage (\ s a -> s{_usvsVersionStage = a})

instance AWSRequest UpdateSecretVersionStage where
        type Rs UpdateSecretVersionStage =
             UpdateSecretVersionStageResponse
        request = postJSON secretsManager
        response
          = receiveJSON
              (\ s h x ->
                 UpdateSecretVersionStageResponse' <$>
                   (x .?> "ARN") <*> (x .?> "Name") <*>
                     (pure (fromEnum s)))

instance Hashable UpdateSecretVersionStage where

instance NFData UpdateSecretVersionStage where

instance ToHeaders UpdateSecretVersionStage where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("secretsmanager.UpdateSecretVersionStage" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateSecretVersionStage where
        toJSON UpdateSecretVersionStage'{..}
          = object
              (catMaybes
                 [("RemoveFromVersionId" .=) <$>
                    _usvsRemoveFromVersionId,
                  ("MoveToVersionId" .=) <$> _usvsMoveToVersionId,
                  Just ("SecretId" .= _usvsSecretId),
                  Just ("VersionStage" .= _usvsVersionStage)])

instance ToPath UpdateSecretVersionStage where
        toPath = const "/"

instance ToQuery UpdateSecretVersionStage where
        toQuery = const mempty

-- | /See:/ 'updateSecretVersionStageResponse' smart constructor.
data UpdateSecretVersionStageResponse = UpdateSecretVersionStageResponse'
  { _usvsrsARN            :: !(Maybe Text)
  , _usvsrsName           :: !(Maybe Text)
  , _usvsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSecretVersionStageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usvsrsARN' - The ARN of the secret with the staging labels that were modified.
--
-- * 'usvsrsName' - The friendly name of the secret with the staging labels that were modified.
--
-- * 'usvsrsResponseStatus' - -- | The response status code.
updateSecretVersionStageResponse
    :: Int -- ^ 'usvsrsResponseStatus'
    -> UpdateSecretVersionStageResponse
updateSecretVersionStageResponse pResponseStatus_ =
  UpdateSecretVersionStageResponse'
    { _usvsrsARN = Nothing
    , _usvsrsName = Nothing
    , _usvsrsResponseStatus = pResponseStatus_
    }


-- | The ARN of the secret with the staging labels that were modified.
usvsrsARN :: Lens' UpdateSecretVersionStageResponse (Maybe Text)
usvsrsARN = lens _usvsrsARN (\ s a -> s{_usvsrsARN = a})

-- | The friendly name of the secret with the staging labels that were modified.
usvsrsName :: Lens' UpdateSecretVersionStageResponse (Maybe Text)
usvsrsName = lens _usvsrsName (\ s a -> s{_usvsrsName = a})

-- | -- | The response status code.
usvsrsResponseStatus :: Lens' UpdateSecretVersionStageResponse Int
usvsrsResponseStatus = lens _usvsrsResponseStatus (\ s a -> s{_usvsrsResponseStatus = a})

instance NFData UpdateSecretVersionStageResponse
         where

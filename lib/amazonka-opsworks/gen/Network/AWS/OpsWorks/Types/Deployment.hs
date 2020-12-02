{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Deployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Deployment where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types.DeploymentCommand
import Network.AWS.Prelude

-- | Describes a deployment of a stack or app.
--
--
--
-- /See:/ 'deployment' smart constructor.
data Deployment = Deployment'
  { _dDeploymentId :: !(Maybe Text),
    _dStatus :: !(Maybe Text),
    _dCommand :: !(Maybe DeploymentCommand),
    _dCreatedAt :: !(Maybe Text),
    _dCustomJSON :: !(Maybe Text),
    _dIAMUserARN :: !(Maybe Text),
    _dAppId :: !(Maybe Text),
    _dInstanceIds :: !(Maybe [Text]),
    _dCompletedAt :: !(Maybe Text),
    _dStackId :: !(Maybe Text),
    _dComment :: !(Maybe Text),
    _dDuration :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Deployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDeploymentId' - The deployment ID.
--
-- * 'dStatus' - The deployment status:     * running     * successful     * failed
--
-- * 'dCommand' - Used to specify a stack or deployment command.
--
-- * 'dCreatedAt' - Date when the deployment was created.
--
-- * 'dCustomJSON' - A string that contains user-defined custom JSON. It can be used to override the corresponding default stack configuration attribute values for stack or to pass data to recipes. The string should be in the following format: @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@  For more information on custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
--
-- * 'dIAMUserARN' - The user's IAM ARN.
--
-- * 'dAppId' - The app ID.
--
-- * 'dInstanceIds' - The IDs of the target instances.
--
-- * 'dCompletedAt' - Date when the deployment completed.
--
-- * 'dStackId' - The stack ID.
--
-- * 'dComment' - A user-defined comment.
--
-- * 'dDuration' - The deployment duration.
deployment ::
  Deployment
deployment =
  Deployment'
    { _dDeploymentId = Nothing,
      _dStatus = Nothing,
      _dCommand = Nothing,
      _dCreatedAt = Nothing,
      _dCustomJSON = Nothing,
      _dIAMUserARN = Nothing,
      _dAppId = Nothing,
      _dInstanceIds = Nothing,
      _dCompletedAt = Nothing,
      _dStackId = Nothing,
      _dComment = Nothing,
      _dDuration = Nothing
    }

-- | The deployment ID.
dDeploymentId :: Lens' Deployment (Maybe Text)
dDeploymentId = lens _dDeploymentId (\s a -> s {_dDeploymentId = a})

-- | The deployment status:     * running     * successful     * failed
dStatus :: Lens' Deployment (Maybe Text)
dStatus = lens _dStatus (\s a -> s {_dStatus = a})

-- | Used to specify a stack or deployment command.
dCommand :: Lens' Deployment (Maybe DeploymentCommand)
dCommand = lens _dCommand (\s a -> s {_dCommand = a})

-- | Date when the deployment was created.
dCreatedAt :: Lens' Deployment (Maybe Text)
dCreatedAt = lens _dCreatedAt (\s a -> s {_dCreatedAt = a})

-- | A string that contains user-defined custom JSON. It can be used to override the corresponding default stack configuration attribute values for stack or to pass data to recipes. The string should be in the following format: @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@  For more information on custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
dCustomJSON :: Lens' Deployment (Maybe Text)
dCustomJSON = lens _dCustomJSON (\s a -> s {_dCustomJSON = a})

-- | The user's IAM ARN.
dIAMUserARN :: Lens' Deployment (Maybe Text)
dIAMUserARN = lens _dIAMUserARN (\s a -> s {_dIAMUserARN = a})

-- | The app ID.
dAppId :: Lens' Deployment (Maybe Text)
dAppId = lens _dAppId (\s a -> s {_dAppId = a})

-- | The IDs of the target instances.
dInstanceIds :: Lens' Deployment [Text]
dInstanceIds = lens _dInstanceIds (\s a -> s {_dInstanceIds = a}) . _Default . _Coerce

-- | Date when the deployment completed.
dCompletedAt :: Lens' Deployment (Maybe Text)
dCompletedAt = lens _dCompletedAt (\s a -> s {_dCompletedAt = a})

-- | The stack ID.
dStackId :: Lens' Deployment (Maybe Text)
dStackId = lens _dStackId (\s a -> s {_dStackId = a})

-- | A user-defined comment.
dComment :: Lens' Deployment (Maybe Text)
dComment = lens _dComment (\s a -> s {_dComment = a})

-- | The deployment duration.
dDuration :: Lens' Deployment (Maybe Int)
dDuration = lens _dDuration (\s a -> s {_dDuration = a})

instance FromJSON Deployment where
  parseJSON =
    withObject
      "Deployment"
      ( \x ->
          Deployment'
            <$> (x .:? "DeploymentId")
            <*> (x .:? "Status")
            <*> (x .:? "Command")
            <*> (x .:? "CreatedAt")
            <*> (x .:? "CustomJson")
            <*> (x .:? "IamUserArn")
            <*> (x .:? "AppId")
            <*> (x .:? "InstanceIds" .!= mempty)
            <*> (x .:? "CompletedAt")
            <*> (x .:? "StackId")
            <*> (x .:? "Comment")
            <*> (x .:? "Duration")
      )

instance Hashable Deployment

instance NFData Deployment

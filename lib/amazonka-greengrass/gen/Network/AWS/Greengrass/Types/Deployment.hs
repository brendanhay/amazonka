{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Deployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Deployment where

import Network.AWS.Greengrass.Types.DeploymentType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a deployment.
--
-- /See:/ 'deployment' smart constructor.
data Deployment = Deployment'
  { _dDeploymentId :: !(Maybe Text),
    _dDeploymentARN :: !(Maybe Text),
    _dCreatedAt :: !(Maybe Text),
    _dDeploymentType :: !(Maybe DeploymentType),
    _dGroupARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Deployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDeploymentId' - The ID of the deployment.
--
-- * 'dDeploymentARN' - The ARN of the deployment.
--
-- * 'dCreatedAt' - The time, in milliseconds since the epoch, when the deployment was created.
--
-- * 'dDeploymentType' - The type of the deployment.
--
-- * 'dGroupARN' - The ARN of the group for this deployment.
deployment ::
  Deployment
deployment =
  Deployment'
    { _dDeploymentId = Nothing,
      _dDeploymentARN = Nothing,
      _dCreatedAt = Nothing,
      _dDeploymentType = Nothing,
      _dGroupARN = Nothing
    }

-- | The ID of the deployment.
dDeploymentId :: Lens' Deployment (Maybe Text)
dDeploymentId = lens _dDeploymentId (\s a -> s {_dDeploymentId = a})

-- | The ARN of the deployment.
dDeploymentARN :: Lens' Deployment (Maybe Text)
dDeploymentARN = lens _dDeploymentARN (\s a -> s {_dDeploymentARN = a})

-- | The time, in milliseconds since the epoch, when the deployment was created.
dCreatedAt :: Lens' Deployment (Maybe Text)
dCreatedAt = lens _dCreatedAt (\s a -> s {_dCreatedAt = a})

-- | The type of the deployment.
dDeploymentType :: Lens' Deployment (Maybe DeploymentType)
dDeploymentType = lens _dDeploymentType (\s a -> s {_dDeploymentType = a})

-- | The ARN of the group for this deployment.
dGroupARN :: Lens' Deployment (Maybe Text)
dGroupARN = lens _dGroupARN (\s a -> s {_dGroupARN = a})

instance FromJSON Deployment where
  parseJSON =
    withObject
      "Deployment"
      ( \x ->
          Deployment'
            <$> (x .:? "DeploymentId")
            <*> (x .:? "DeploymentArn")
            <*> (x .:? "CreatedAt")
            <*> (x .:? "DeploymentType")
            <*> (x .:? "GroupArn")
      )

instance Hashable Deployment

instance NFData Deployment

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Deployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.Deployment where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an application version deployment.
--
--
--
-- /See:/ 'deployment' smart constructor.
data Deployment = Deployment'
  { _dDeploymentId :: !(Maybe Integer),
    _dStatus :: !(Maybe Text),
    _dDeploymentTime :: !(Maybe ISO8601),
    _dVersionLabel :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Deployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDeploymentId' - The ID of the deployment. This number increases by one each time that you deploy source code or change instance configuration settings.
--
-- * 'dStatus' - The status of the deployment:     * @In Progress@ : The deployment is in progress.     * @Deployed@ : The deployment succeeded.     * @Failed@ : The deployment failed.
--
-- * 'dDeploymentTime' - For in-progress deployments, the time that the deployment started. For completed deployments, the time that the deployment ended.
--
-- * 'dVersionLabel' - The version label of the application version in the deployment.
deployment ::
  Deployment
deployment =
  Deployment'
    { _dDeploymentId = Nothing,
      _dStatus = Nothing,
      _dDeploymentTime = Nothing,
      _dVersionLabel = Nothing
    }

-- | The ID of the deployment. This number increases by one each time that you deploy source code or change instance configuration settings.
dDeploymentId :: Lens' Deployment (Maybe Integer)
dDeploymentId = lens _dDeploymentId (\s a -> s {_dDeploymentId = a})

-- | The status of the deployment:     * @In Progress@ : The deployment is in progress.     * @Deployed@ : The deployment succeeded.     * @Failed@ : The deployment failed.
dStatus :: Lens' Deployment (Maybe Text)
dStatus = lens _dStatus (\s a -> s {_dStatus = a})

-- | For in-progress deployments, the time that the deployment started. For completed deployments, the time that the deployment ended.
dDeploymentTime :: Lens' Deployment (Maybe UTCTime)
dDeploymentTime = lens _dDeploymentTime (\s a -> s {_dDeploymentTime = a}) . mapping _Time

-- | The version label of the application version in the deployment.
dVersionLabel :: Lens' Deployment (Maybe Text)
dVersionLabel = lens _dVersionLabel (\s a -> s {_dVersionLabel = a})

instance FromXML Deployment where
  parseXML x =
    Deployment'
      <$> (x .@? "DeploymentId")
      <*> (x .@? "Status")
      <*> (x .@? "DeploymentTime")
      <*> (x .@? "VersionLabel")

instance Hashable Deployment

instance NFData Deployment

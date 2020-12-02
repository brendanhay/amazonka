{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.LastDeploymentInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LastDeploymentInfo where

import Network.AWS.CodeDeploy.Types.DeploymentStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the most recent attempted or successful deployment to a deployment group.
--
--
--
-- /See:/ 'lastDeploymentInfo' smart constructor.
data LastDeploymentInfo = LastDeploymentInfo'
  { _ldiStatus ::
      !(Maybe DeploymentStatus),
    _ldiDeploymentId :: !(Maybe Text),
    _ldiEndTime :: !(Maybe POSIX),
    _ldiCreateTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LastDeploymentInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldiStatus' - The status of the most recent deployment.
--
-- * 'ldiDeploymentId' - The unique ID of a deployment.
--
-- * 'ldiEndTime' - A timestamp that indicates when the most recent deployment to the deployment group was complete.
--
-- * 'ldiCreateTime' - A timestamp that indicates when the most recent deployment to the deployment group started.
lastDeploymentInfo ::
  LastDeploymentInfo
lastDeploymentInfo =
  LastDeploymentInfo'
    { _ldiStatus = Nothing,
      _ldiDeploymentId = Nothing,
      _ldiEndTime = Nothing,
      _ldiCreateTime = Nothing
    }

-- | The status of the most recent deployment.
ldiStatus :: Lens' LastDeploymentInfo (Maybe DeploymentStatus)
ldiStatus = lens _ldiStatus (\s a -> s {_ldiStatus = a})

-- | The unique ID of a deployment.
ldiDeploymentId :: Lens' LastDeploymentInfo (Maybe Text)
ldiDeploymentId = lens _ldiDeploymentId (\s a -> s {_ldiDeploymentId = a})

-- | A timestamp that indicates when the most recent deployment to the deployment group was complete.
ldiEndTime :: Lens' LastDeploymentInfo (Maybe UTCTime)
ldiEndTime = lens _ldiEndTime (\s a -> s {_ldiEndTime = a}) . mapping _Time

-- | A timestamp that indicates when the most recent deployment to the deployment group started.
ldiCreateTime :: Lens' LastDeploymentInfo (Maybe UTCTime)
ldiCreateTime = lens _ldiCreateTime (\s a -> s {_ldiCreateTime = a}) . mapping _Time

instance FromJSON LastDeploymentInfo where
  parseJSON =
    withObject
      "LastDeploymentInfo"
      ( \x ->
          LastDeploymentInfo'
            <$> (x .:? "status")
            <*> (x .:? "deploymentId")
            <*> (x .:? "endTime")
            <*> (x .:? "createTime")
      )

instance Hashable LastDeploymentInfo

instance NFData LastDeploymentInfo

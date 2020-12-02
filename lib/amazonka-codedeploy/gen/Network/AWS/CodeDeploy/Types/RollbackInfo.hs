{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.RollbackInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.RollbackInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a deployment rollback.
--
--
--
-- /See:/ 'rollbackInfo' smart constructor.
data RollbackInfo = RollbackInfo'
  { _riRollbackTriggeringDeploymentId ::
      !(Maybe Text),
    _riRollbackMessage :: !(Maybe Text),
    _riRollbackDeploymentId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RollbackInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riRollbackTriggeringDeploymentId' - The deployment ID of the deployment that was underway and triggered a rollback deployment because it failed or was stopped.
--
-- * 'riRollbackMessage' - Information that describes the status of a deployment rollback (for example, whether the deployment can't be rolled back, is in progress, failed, or succeeded).
--
-- * 'riRollbackDeploymentId' - The ID of the deployment rollback.
rollbackInfo ::
  RollbackInfo
rollbackInfo =
  RollbackInfo'
    { _riRollbackTriggeringDeploymentId = Nothing,
      _riRollbackMessage = Nothing,
      _riRollbackDeploymentId = Nothing
    }

-- | The deployment ID of the deployment that was underway and triggered a rollback deployment because it failed or was stopped.
riRollbackTriggeringDeploymentId :: Lens' RollbackInfo (Maybe Text)
riRollbackTriggeringDeploymentId = lens _riRollbackTriggeringDeploymentId (\s a -> s {_riRollbackTriggeringDeploymentId = a})

-- | Information that describes the status of a deployment rollback (for example, whether the deployment can't be rolled back, is in progress, failed, or succeeded).
riRollbackMessage :: Lens' RollbackInfo (Maybe Text)
riRollbackMessage = lens _riRollbackMessage (\s a -> s {_riRollbackMessage = a})

-- | The ID of the deployment rollback.
riRollbackDeploymentId :: Lens' RollbackInfo (Maybe Text)
riRollbackDeploymentId = lens _riRollbackDeploymentId (\s a -> s {_riRollbackDeploymentId = a})

instance FromJSON RollbackInfo where
  parseJSON =
    withObject
      "RollbackInfo"
      ( \x ->
          RollbackInfo'
            <$> (x .:? "rollbackTriggeringDeploymentId")
            <*> (x .:? "rollbackMessage")
            <*> (x .:? "rollbackDeploymentId")
      )

instance Hashable RollbackInfo

instance NFData RollbackInfo

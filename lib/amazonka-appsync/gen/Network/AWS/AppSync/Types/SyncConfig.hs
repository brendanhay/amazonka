{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.SyncConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.SyncConfig where

import Network.AWS.AppSync.Types.ConflictDetectionType
import Network.AWS.AppSync.Types.ConflictHandlerType
import Network.AWS.AppSync.Types.LambdaConflictHandlerConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a Sync configuration for a resolver.
--
--
-- Contains information on which Conflict Detection as well as Resolution strategy should be performed when the resolver is invoked.
--
--
-- /See:/ 'syncConfig' smart constructor.
data SyncConfig = SyncConfig'
  { _scConflictHandler ::
      !(Maybe ConflictHandlerType),
    _scConflictDetection :: !(Maybe ConflictDetectionType),
    _scLambdaConflictHandlerConfig ::
      !(Maybe LambdaConflictHandlerConfig)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SyncConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scConflictHandler' - The Conflict Resolution strategy to perform in the event of a conflict.     * __OPTIMISTIC_CONCURRENCY__ : Resolve conflicts by rejecting mutations when versions do not match the latest version at the server.     * __AUTOMERGE__ : Resolve conflicts with the Automerge conflict resolution strategy.     * __LAMBDA__ : Resolve conflicts with a Lambda function supplied in the LambdaConflictHandlerConfig.
--
-- * 'scConflictDetection' - The Conflict Detection strategy to use.     * __VERSION__ : Detect conflicts based on object versions for this resolver.     * __NONE__ : Do not detect conflicts when executing this resolver.
--
-- * 'scLambdaConflictHandlerConfig' - The @LambdaConflictHandlerConfig@ when configuring LAMBDA as the Conflict Handler.
syncConfig ::
  SyncConfig
syncConfig =
  SyncConfig'
    { _scConflictHandler = Nothing,
      _scConflictDetection = Nothing,
      _scLambdaConflictHandlerConfig = Nothing
    }

-- | The Conflict Resolution strategy to perform in the event of a conflict.     * __OPTIMISTIC_CONCURRENCY__ : Resolve conflicts by rejecting mutations when versions do not match the latest version at the server.     * __AUTOMERGE__ : Resolve conflicts with the Automerge conflict resolution strategy.     * __LAMBDA__ : Resolve conflicts with a Lambda function supplied in the LambdaConflictHandlerConfig.
scConflictHandler :: Lens' SyncConfig (Maybe ConflictHandlerType)
scConflictHandler = lens _scConflictHandler (\s a -> s {_scConflictHandler = a})

-- | The Conflict Detection strategy to use.     * __VERSION__ : Detect conflicts based on object versions for this resolver.     * __NONE__ : Do not detect conflicts when executing this resolver.
scConflictDetection :: Lens' SyncConfig (Maybe ConflictDetectionType)
scConflictDetection = lens _scConflictDetection (\s a -> s {_scConflictDetection = a})

-- | The @LambdaConflictHandlerConfig@ when configuring LAMBDA as the Conflict Handler.
scLambdaConflictHandlerConfig :: Lens' SyncConfig (Maybe LambdaConflictHandlerConfig)
scLambdaConflictHandlerConfig = lens _scLambdaConflictHandlerConfig (\s a -> s {_scLambdaConflictHandlerConfig = a})

instance FromJSON SyncConfig where
  parseJSON =
    withObject
      "SyncConfig"
      ( \x ->
          SyncConfig'
            <$> (x .:? "conflictHandler")
            <*> (x .:? "conflictDetection")
            <*> (x .:? "lambdaConflictHandlerConfig")
      )

instance Hashable SyncConfig

instance NFData SyncConfig

instance ToJSON SyncConfig where
  toJSON SyncConfig' {..} =
    object
      ( catMaybes
          [ ("conflictHandler" .=) <$> _scConflictHandler,
            ("conflictDetection" .=) <$> _scConflictDetection,
            ("lambdaConflictHandlerConfig" .=)
              <$> _scLambdaConflictHandlerConfig
          ]
      )

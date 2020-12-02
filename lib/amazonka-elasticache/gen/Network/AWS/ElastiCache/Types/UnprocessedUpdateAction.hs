{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.UnprocessedUpdateAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.UnprocessedUpdateAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Update action that has failed to be processed for the corresponding apply/stop request
--
--
--
-- /See:/ 'unprocessedUpdateAction' smart constructor.
data UnprocessedUpdateAction = UnprocessedUpdateAction'
  { _uuaCacheClusterId ::
      !(Maybe Text),
    _uuaServiceUpdateName :: !(Maybe Text),
    _uuaErrorType :: !(Maybe Text),
    _uuaErrorMessage :: !(Maybe Text),
    _uuaReplicationGroupId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UnprocessedUpdateAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uuaCacheClusterId' - The ID of the cache cluster
--
-- * 'uuaServiceUpdateName' - The unique ID of the service update
--
-- * 'uuaErrorType' - The error type for requests that are not processed
--
-- * 'uuaErrorMessage' - The error message that describes the reason the request was not processed
--
-- * 'uuaReplicationGroupId' - The replication group ID
unprocessedUpdateAction ::
  UnprocessedUpdateAction
unprocessedUpdateAction =
  UnprocessedUpdateAction'
    { _uuaCacheClusterId = Nothing,
      _uuaServiceUpdateName = Nothing,
      _uuaErrorType = Nothing,
      _uuaErrorMessage = Nothing,
      _uuaReplicationGroupId = Nothing
    }

-- | The ID of the cache cluster
uuaCacheClusterId :: Lens' UnprocessedUpdateAction (Maybe Text)
uuaCacheClusterId = lens _uuaCacheClusterId (\s a -> s {_uuaCacheClusterId = a})

-- | The unique ID of the service update
uuaServiceUpdateName :: Lens' UnprocessedUpdateAction (Maybe Text)
uuaServiceUpdateName = lens _uuaServiceUpdateName (\s a -> s {_uuaServiceUpdateName = a})

-- | The error type for requests that are not processed
uuaErrorType :: Lens' UnprocessedUpdateAction (Maybe Text)
uuaErrorType = lens _uuaErrorType (\s a -> s {_uuaErrorType = a})

-- | The error message that describes the reason the request was not processed
uuaErrorMessage :: Lens' UnprocessedUpdateAction (Maybe Text)
uuaErrorMessage = lens _uuaErrorMessage (\s a -> s {_uuaErrorMessage = a})

-- | The replication group ID
uuaReplicationGroupId :: Lens' UnprocessedUpdateAction (Maybe Text)
uuaReplicationGroupId = lens _uuaReplicationGroupId (\s a -> s {_uuaReplicationGroupId = a})

instance FromXML UnprocessedUpdateAction where
  parseXML x =
    UnprocessedUpdateAction'
      <$> (x .@? "CacheClusterId")
      <*> (x .@? "ServiceUpdateName")
      <*> (x .@? "ErrorType")
      <*> (x .@? "ErrorMessage")
      <*> (x .@? "ReplicationGroupId")

instance Hashable UnprocessedUpdateAction

instance NFData UnprocessedUpdateAction

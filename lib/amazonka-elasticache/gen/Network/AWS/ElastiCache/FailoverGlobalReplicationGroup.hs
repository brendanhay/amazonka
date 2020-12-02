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
-- Module      : Network.AWS.ElastiCache.FailoverGlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to failover the primary region to a selected secondary region. The selected secondary region will become primary, and all other clusters will become secondary.
module Network.AWS.ElastiCache.FailoverGlobalReplicationGroup
  ( -- * Creating a Request
    failoverGlobalReplicationGroup,
    FailoverGlobalReplicationGroup,

    -- * Request Lenses
    fgrgGlobalReplicationGroupId,
    fgrgPrimaryRegion,
    fgrgPrimaryReplicationGroupId,

    -- * Destructuring the Response
    failoverGlobalReplicationGroupResponse,
    FailoverGlobalReplicationGroupResponse,

    -- * Response Lenses
    fgrgrsGlobalReplicationGroup,
    fgrgrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'failoverGlobalReplicationGroup' smart constructor.
data FailoverGlobalReplicationGroup = FailoverGlobalReplicationGroup'
  { _fgrgGlobalReplicationGroupId ::
      !Text,
    _fgrgPrimaryRegion :: !Text,
    _fgrgPrimaryReplicationGroupId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FailoverGlobalReplicationGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fgrgGlobalReplicationGroupId' - The name of the Global Datastore
--
-- * 'fgrgPrimaryRegion' - The AWS region of the primary cluster of the Global Datastore
--
-- * 'fgrgPrimaryReplicationGroupId' - The name of the primary replication group
failoverGlobalReplicationGroup ::
  -- | 'fgrgGlobalReplicationGroupId'
  Text ->
  -- | 'fgrgPrimaryRegion'
  Text ->
  -- | 'fgrgPrimaryReplicationGroupId'
  Text ->
  FailoverGlobalReplicationGroup
failoverGlobalReplicationGroup
  pGlobalReplicationGroupId_
  pPrimaryRegion_
  pPrimaryReplicationGroupId_ =
    FailoverGlobalReplicationGroup'
      { _fgrgGlobalReplicationGroupId =
          pGlobalReplicationGroupId_,
        _fgrgPrimaryRegion = pPrimaryRegion_,
        _fgrgPrimaryReplicationGroupId = pPrimaryReplicationGroupId_
      }

-- | The name of the Global Datastore
fgrgGlobalReplicationGroupId :: Lens' FailoverGlobalReplicationGroup Text
fgrgGlobalReplicationGroupId = lens _fgrgGlobalReplicationGroupId (\s a -> s {_fgrgGlobalReplicationGroupId = a})

-- | The AWS region of the primary cluster of the Global Datastore
fgrgPrimaryRegion :: Lens' FailoverGlobalReplicationGroup Text
fgrgPrimaryRegion = lens _fgrgPrimaryRegion (\s a -> s {_fgrgPrimaryRegion = a})

-- | The name of the primary replication group
fgrgPrimaryReplicationGroupId :: Lens' FailoverGlobalReplicationGroup Text
fgrgPrimaryReplicationGroupId = lens _fgrgPrimaryReplicationGroupId (\s a -> s {_fgrgPrimaryReplicationGroupId = a})

instance AWSRequest FailoverGlobalReplicationGroup where
  type
    Rs FailoverGlobalReplicationGroup =
      FailoverGlobalReplicationGroupResponse
  request = postQuery elastiCache
  response =
    receiveXMLWrapper
      "FailoverGlobalReplicationGroupResult"
      ( \s h x ->
          FailoverGlobalReplicationGroupResponse'
            <$> (x .@? "GlobalReplicationGroup") <*> (pure (fromEnum s))
      )

instance Hashable FailoverGlobalReplicationGroup

instance NFData FailoverGlobalReplicationGroup

instance ToHeaders FailoverGlobalReplicationGroup where
  toHeaders = const mempty

instance ToPath FailoverGlobalReplicationGroup where
  toPath = const "/"

instance ToQuery FailoverGlobalReplicationGroup where
  toQuery FailoverGlobalReplicationGroup' {..} =
    mconcat
      [ "Action" =: ("FailoverGlobalReplicationGroup" :: ByteString),
        "Version" =: ("2015-02-02" :: ByteString),
        "GlobalReplicationGroupId" =: _fgrgGlobalReplicationGroupId,
        "PrimaryRegion" =: _fgrgPrimaryRegion,
        "PrimaryReplicationGroupId" =: _fgrgPrimaryReplicationGroupId
      ]

-- | /See:/ 'failoverGlobalReplicationGroupResponse' smart constructor.
data FailoverGlobalReplicationGroupResponse = FailoverGlobalReplicationGroupResponse'
  { _fgrgrsGlobalReplicationGroup ::
      !( Maybe
           GlobalReplicationGroup
       ),
    _fgrgrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FailoverGlobalReplicationGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fgrgrsGlobalReplicationGroup' - Undocumented member.
--
-- * 'fgrgrsResponseStatus' - -- | The response status code.
failoverGlobalReplicationGroupResponse ::
  -- | 'fgrgrsResponseStatus'
  Int ->
  FailoverGlobalReplicationGroupResponse
failoverGlobalReplicationGroupResponse pResponseStatus_ =
  FailoverGlobalReplicationGroupResponse'
    { _fgrgrsGlobalReplicationGroup =
        Nothing,
      _fgrgrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
fgrgrsGlobalReplicationGroup :: Lens' FailoverGlobalReplicationGroupResponse (Maybe GlobalReplicationGroup)
fgrgrsGlobalReplicationGroup = lens _fgrgrsGlobalReplicationGroup (\s a -> s {_fgrgrsGlobalReplicationGroup = a})

-- | -- | The response status code.
fgrgrsResponseStatus :: Lens' FailoverGlobalReplicationGroupResponse Int
fgrgrsResponseStatus = lens _fgrgrsResponseStatus (\s a -> s {_fgrgrsResponseStatus = a})

instance NFData FailoverGlobalReplicationGroupResponse

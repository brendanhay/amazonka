-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.SourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.SourceType
  ( SourceType
      ( SourceType',
        CacheCluster,
        CacheParameterGroup,
        CacheSecurityGroup,
        CacheSubnetGroup,
        ReplicationGroup,
        User,
        UserGroup
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SourceType = SourceType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CacheCluster :: SourceType
pattern CacheCluster = SourceType' "cache-cluster"

pattern CacheParameterGroup :: SourceType
pattern CacheParameterGroup = SourceType' "cache-parameter-group"

pattern CacheSecurityGroup :: SourceType
pattern CacheSecurityGroup = SourceType' "cache-security-group"

pattern CacheSubnetGroup :: SourceType
pattern CacheSubnetGroup = SourceType' "cache-subnet-group"

pattern ReplicationGroup :: SourceType
pattern ReplicationGroup = SourceType' "replication-group"

pattern User :: SourceType
pattern User = SourceType' "user"

pattern UserGroup :: SourceType
pattern UserGroup = SourceType' "user-group"

{-# COMPLETE
  CacheCluster,
  CacheParameterGroup,
  CacheSecurityGroup,
  CacheSubnetGroup,
  ReplicationGroup,
  User,
  UserGroup,
  SourceType'
  #-}

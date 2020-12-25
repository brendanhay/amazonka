{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        SourceTypeCacheCluster,
        SourceTypeCacheParameterGroup,
        SourceTypeCacheSecurityGroup,
        SourceTypeCacheSubnetGroup,
        SourceTypeReplicationGroup,
        SourceTypeUser,
        SourceTypeUserGroup,
        fromSourceType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype SourceType = SourceType' {fromSourceType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern SourceTypeCacheCluster :: SourceType
pattern SourceTypeCacheCluster = SourceType' "cache-cluster"

pattern SourceTypeCacheParameterGroup :: SourceType
pattern SourceTypeCacheParameterGroup = SourceType' "cache-parameter-group"

pattern SourceTypeCacheSecurityGroup :: SourceType
pattern SourceTypeCacheSecurityGroup = SourceType' "cache-security-group"

pattern SourceTypeCacheSubnetGroup :: SourceType
pattern SourceTypeCacheSubnetGroup = SourceType' "cache-subnet-group"

pattern SourceTypeReplicationGroup :: SourceType
pattern SourceTypeReplicationGroup = SourceType' "replication-group"

pattern SourceTypeUser :: SourceType
pattern SourceTypeUser = SourceType' "user"

pattern SourceTypeUserGroup :: SourceType
pattern SourceTypeUserGroup = SourceType' "user-group"

{-# COMPLETE
  SourceTypeCacheCluster,
  SourceTypeCacheParameterGroup,
  SourceTypeCacheSecurityGroup,
  SourceTypeCacheSubnetGroup,
  SourceTypeReplicationGroup,
  SourceTypeUser,
  SourceTypeUserGroup,
  SourceType'
  #-}

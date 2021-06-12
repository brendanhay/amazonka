{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.SourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.SourceType
  ( SourceType
      ( ..,
        SourceType_Cache_cluster,
        SourceType_Cache_parameter_group,
        SourceType_Cache_security_group,
        SourceType_Cache_subnet_group,
        SourceType_Replication_group,
        SourceType_User,
        SourceType_User_group
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype SourceType = SourceType'
  { fromSourceType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern SourceType_Cache_cluster :: SourceType
pattern SourceType_Cache_cluster = SourceType' "cache-cluster"

pattern SourceType_Cache_parameter_group :: SourceType
pattern SourceType_Cache_parameter_group = SourceType' "cache-parameter-group"

pattern SourceType_Cache_security_group :: SourceType
pattern SourceType_Cache_security_group = SourceType' "cache-security-group"

pattern SourceType_Cache_subnet_group :: SourceType
pattern SourceType_Cache_subnet_group = SourceType' "cache-subnet-group"

pattern SourceType_Replication_group :: SourceType
pattern SourceType_Replication_group = SourceType' "replication-group"

pattern SourceType_User :: SourceType
pattern SourceType_User = SourceType' "user"

pattern SourceType_User_group :: SourceType
pattern SourceType_User_group = SourceType' "user-group"

{-# COMPLETE
  SourceType_Cache_cluster,
  SourceType_Cache_parameter_group,
  SourceType_Cache_security_group,
  SourceType_Cache_subnet_group,
  SourceType_Replication_group,
  SourceType_User,
  SourceType_User_group,
  SourceType'
  #-}

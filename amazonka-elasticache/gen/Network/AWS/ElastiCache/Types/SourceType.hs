{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype SourceType = SourceType'
  { fromSourceType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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

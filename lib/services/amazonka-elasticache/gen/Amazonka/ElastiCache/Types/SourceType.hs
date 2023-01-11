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
-- Module      : Amazonka.ElastiCache.Types.SourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.SourceType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SourceType = SourceType'
  { fromSourceType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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

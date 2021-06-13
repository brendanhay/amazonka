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
-- Module      : Network.AWS.RDS.Types.SourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.SourceType
  ( SourceType
      ( ..,
        SourceType_Db_cluster,
        SourceType_Db_cluster_snapshot,
        SourceType_Db_instance,
        SourceType_Db_parameter_group,
        SourceType_Db_security_group,
        SourceType_Db_snapshot
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype SourceType = SourceType'
  { fromSourceType ::
      Core.Text
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

pattern SourceType_Db_cluster :: SourceType
pattern SourceType_Db_cluster = SourceType' "db-cluster"

pattern SourceType_Db_cluster_snapshot :: SourceType
pattern SourceType_Db_cluster_snapshot = SourceType' "db-cluster-snapshot"

pattern SourceType_Db_instance :: SourceType
pattern SourceType_Db_instance = SourceType' "db-instance"

pattern SourceType_Db_parameter_group :: SourceType
pattern SourceType_Db_parameter_group = SourceType' "db-parameter-group"

pattern SourceType_Db_security_group :: SourceType
pattern SourceType_Db_security_group = SourceType' "db-security-group"

pattern SourceType_Db_snapshot :: SourceType
pattern SourceType_Db_snapshot = SourceType' "db-snapshot"

{-# COMPLETE
  SourceType_Db_cluster,
  SourceType_Db_cluster_snapshot,
  SourceType_Db_instance,
  SourceType_Db_parameter_group,
  SourceType_Db_security_group,
  SourceType_Db_snapshot,
  SourceType'
  #-}

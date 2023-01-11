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
-- Module      : Amazonka.Redshift.Types.SourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.SourceType
  ( SourceType
      ( ..,
        SourceType_Cluster,
        SourceType_Cluster_parameter_group,
        SourceType_Cluster_security_group,
        SourceType_Cluster_snapshot,
        SourceType_Scheduled_action
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

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

pattern SourceType_Cluster :: SourceType
pattern SourceType_Cluster = SourceType' "cluster"

pattern SourceType_Cluster_parameter_group :: SourceType
pattern SourceType_Cluster_parameter_group = SourceType' "cluster-parameter-group"

pattern SourceType_Cluster_security_group :: SourceType
pattern SourceType_Cluster_security_group = SourceType' "cluster-security-group"

pattern SourceType_Cluster_snapshot :: SourceType
pattern SourceType_Cluster_snapshot = SourceType' "cluster-snapshot"

pattern SourceType_Scheduled_action :: SourceType
pattern SourceType_Scheduled_action = SourceType' "scheduled-action"

{-# COMPLETE
  SourceType_Cluster,
  SourceType_Cluster_parameter_group,
  SourceType_Cluster_security_group,
  SourceType_Cluster_snapshot,
  SourceType_Scheduled_action,
  SourceType'
  #-}

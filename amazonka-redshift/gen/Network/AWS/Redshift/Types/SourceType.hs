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
-- Module      : Network.AWS.Redshift.Types.SourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.SourceType
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

import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

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

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
-- Module      : Amazonka.ECS.Types.ClusterField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ClusterField
  ( ClusterField
      ( ..,
        ClusterField_ATTACHMENTS,
        ClusterField_CONFIGURATIONS,
        ClusterField_SETTINGS,
        ClusterField_STATISTICS,
        ClusterField_TAGS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ClusterField = ClusterField'
  { fromClusterField ::
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

pattern ClusterField_ATTACHMENTS :: ClusterField
pattern ClusterField_ATTACHMENTS = ClusterField' "ATTACHMENTS"

pattern ClusterField_CONFIGURATIONS :: ClusterField
pattern ClusterField_CONFIGURATIONS = ClusterField' "CONFIGURATIONS"

pattern ClusterField_SETTINGS :: ClusterField
pattern ClusterField_SETTINGS = ClusterField' "SETTINGS"

pattern ClusterField_STATISTICS :: ClusterField
pattern ClusterField_STATISTICS = ClusterField' "STATISTICS"

pattern ClusterField_TAGS :: ClusterField
pattern ClusterField_TAGS = ClusterField' "TAGS"

{-# COMPLETE
  ClusterField_ATTACHMENTS,
  ClusterField_CONFIGURATIONS,
  ClusterField_SETTINGS,
  ClusterField_STATISTICS,
  ClusterField_TAGS,
  ClusterField'
  #-}

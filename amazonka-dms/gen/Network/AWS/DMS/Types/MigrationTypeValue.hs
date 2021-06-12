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
-- Module      : Network.AWS.DMS.Types.MigrationTypeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.MigrationTypeValue
  ( MigrationTypeValue
      ( ..,
        MigrationTypeValue_Cdc,
        MigrationTypeValue_Full_load,
        MigrationTypeValue_Full_load_and_cdc
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype MigrationTypeValue = MigrationTypeValue'
  { fromMigrationTypeValue ::
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

pattern MigrationTypeValue_Cdc :: MigrationTypeValue
pattern MigrationTypeValue_Cdc = MigrationTypeValue' "cdc"

pattern MigrationTypeValue_Full_load :: MigrationTypeValue
pattern MigrationTypeValue_Full_load = MigrationTypeValue' "full-load"

pattern MigrationTypeValue_Full_load_and_cdc :: MigrationTypeValue
pattern MigrationTypeValue_Full_load_and_cdc = MigrationTypeValue' "full-load-and-cdc"

{-# COMPLETE
  MigrationTypeValue_Cdc,
  MigrationTypeValue_Full_load,
  MigrationTypeValue_Full_load_and_cdc,
  MigrationTypeValue'
  #-}

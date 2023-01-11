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
-- Module      : Amazonka.DMS.Types.MigrationTypeValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.MigrationTypeValue
  ( MigrationTypeValue
      ( ..,
        MigrationTypeValue_Cdc,
        MigrationTypeValue_Full_load,
        MigrationTypeValue_Full_load_and_cdc
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MigrationTypeValue = MigrationTypeValue'
  { fromMigrationTypeValue ::
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

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
-- Module      : Amazonka.SecurityLake.Types.SettingsStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.SettingsStatus
  ( SettingsStatus
      ( ..,
        SettingsStatus_COMPLETED,
        SettingsStatus_FAILED,
        SettingsStatus_INITIALIZED,
        SettingsStatus_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SettingsStatus = SettingsStatus'
  { fromSettingsStatus ::
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

pattern SettingsStatus_COMPLETED :: SettingsStatus
pattern SettingsStatus_COMPLETED = SettingsStatus' "COMPLETED"

pattern SettingsStatus_FAILED :: SettingsStatus
pattern SettingsStatus_FAILED = SettingsStatus' "FAILED"

pattern SettingsStatus_INITIALIZED :: SettingsStatus
pattern SettingsStatus_INITIALIZED = SettingsStatus' "INITIALIZED"

pattern SettingsStatus_PENDING :: SettingsStatus
pattern SettingsStatus_PENDING = SettingsStatus' "PENDING"

{-# COMPLETE
  SettingsStatus_COMPLETED,
  SettingsStatus_FAILED,
  SettingsStatus_INITIALIZED,
  SettingsStatus_PENDING,
  SettingsStatus'
  #-}

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
-- Module      : Amazonka.NetworkFirewall.Types.ConfigurationSyncState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.ConfigurationSyncState
  ( ConfigurationSyncState
      ( ..,
        ConfigurationSyncState_CAPACITY_CONSTRAINED,
        ConfigurationSyncState_IN_SYNC,
        ConfigurationSyncState_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConfigurationSyncState = ConfigurationSyncState'
  { fromConfigurationSyncState ::
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

pattern ConfigurationSyncState_CAPACITY_CONSTRAINED :: ConfigurationSyncState
pattern ConfigurationSyncState_CAPACITY_CONSTRAINED = ConfigurationSyncState' "CAPACITY_CONSTRAINED"

pattern ConfigurationSyncState_IN_SYNC :: ConfigurationSyncState
pattern ConfigurationSyncState_IN_SYNC = ConfigurationSyncState' "IN_SYNC"

pattern ConfigurationSyncState_PENDING :: ConfigurationSyncState
pattern ConfigurationSyncState_PENDING = ConfigurationSyncState' "PENDING"

{-# COMPLETE
  ConfigurationSyncState_CAPACITY_CONSTRAINED,
  ConfigurationSyncState_IN_SYNC,
  ConfigurationSyncState_PENDING,
  ConfigurationSyncState'
  #-}

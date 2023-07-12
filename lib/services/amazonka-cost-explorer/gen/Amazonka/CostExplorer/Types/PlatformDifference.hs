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
-- Module      : Amazonka.CostExplorer.Types.PlatformDifference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.PlatformDifference
  ( PlatformDifference
      ( ..,
        PlatformDifference_HYPERVISOR,
        PlatformDifference_INSTANCE_STORE_AVAILABILITY,
        PlatformDifference_NETWORK_INTERFACE,
        PlatformDifference_STORAGE_INTERFACE,
        PlatformDifference_VIRTUALIZATION_TYPE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PlatformDifference = PlatformDifference'
  { fromPlatformDifference ::
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

pattern PlatformDifference_HYPERVISOR :: PlatformDifference
pattern PlatformDifference_HYPERVISOR = PlatformDifference' "HYPERVISOR"

pattern PlatformDifference_INSTANCE_STORE_AVAILABILITY :: PlatformDifference
pattern PlatformDifference_INSTANCE_STORE_AVAILABILITY = PlatformDifference' "INSTANCE_STORE_AVAILABILITY"

pattern PlatformDifference_NETWORK_INTERFACE :: PlatformDifference
pattern PlatformDifference_NETWORK_INTERFACE = PlatformDifference' "NETWORK_INTERFACE"

pattern PlatformDifference_STORAGE_INTERFACE :: PlatformDifference
pattern PlatformDifference_STORAGE_INTERFACE = PlatformDifference' "STORAGE_INTERFACE"

pattern PlatformDifference_VIRTUALIZATION_TYPE :: PlatformDifference
pattern PlatformDifference_VIRTUALIZATION_TYPE = PlatformDifference' "VIRTUALIZATION_TYPE"

{-# COMPLETE
  PlatformDifference_HYPERVISOR,
  PlatformDifference_INSTANCE_STORE_AVAILABILITY,
  PlatformDifference_NETWORK_INTERFACE,
  PlatformDifference_STORAGE_INTERFACE,
  PlatformDifference_VIRTUALIZATION_TYPE,
  PlatformDifference'
  #-}

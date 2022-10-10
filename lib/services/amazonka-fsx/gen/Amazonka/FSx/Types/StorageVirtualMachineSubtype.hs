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
-- Module      : Amazonka.FSx.Types.StorageVirtualMachineSubtype
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.StorageVirtualMachineSubtype
  ( StorageVirtualMachineSubtype
      ( ..,
        StorageVirtualMachineSubtype_DEFAULT,
        StorageVirtualMachineSubtype_DP_DESTINATION,
        StorageVirtualMachineSubtype_SYNC_DESTINATION,
        StorageVirtualMachineSubtype_SYNC_SOURCE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype StorageVirtualMachineSubtype = StorageVirtualMachineSubtype'
  { fromStorageVirtualMachineSubtype ::
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

pattern StorageVirtualMachineSubtype_DEFAULT :: StorageVirtualMachineSubtype
pattern StorageVirtualMachineSubtype_DEFAULT = StorageVirtualMachineSubtype' "DEFAULT"

pattern StorageVirtualMachineSubtype_DP_DESTINATION :: StorageVirtualMachineSubtype
pattern StorageVirtualMachineSubtype_DP_DESTINATION = StorageVirtualMachineSubtype' "DP_DESTINATION"

pattern StorageVirtualMachineSubtype_SYNC_DESTINATION :: StorageVirtualMachineSubtype
pattern StorageVirtualMachineSubtype_SYNC_DESTINATION = StorageVirtualMachineSubtype' "SYNC_DESTINATION"

pattern StorageVirtualMachineSubtype_SYNC_SOURCE :: StorageVirtualMachineSubtype
pattern StorageVirtualMachineSubtype_SYNC_SOURCE = StorageVirtualMachineSubtype' "SYNC_SOURCE"

{-# COMPLETE
  StorageVirtualMachineSubtype_DEFAULT,
  StorageVirtualMachineSubtype_DP_DESTINATION,
  StorageVirtualMachineSubtype_SYNC_DESTINATION,
  StorageVirtualMachineSubtype_SYNC_SOURCE,
  StorageVirtualMachineSubtype'
  #-}

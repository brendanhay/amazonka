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
-- Module      : Amazonka.FSx.Types.StorageVirtualMachineLifecycle
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.StorageVirtualMachineLifecycle
  ( StorageVirtualMachineLifecycle
      ( ..,
        StorageVirtualMachineLifecycle_CREATED,
        StorageVirtualMachineLifecycle_CREATING,
        StorageVirtualMachineLifecycle_DELETING,
        StorageVirtualMachineLifecycle_FAILED,
        StorageVirtualMachineLifecycle_MISCONFIGURED,
        StorageVirtualMachineLifecycle_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype StorageVirtualMachineLifecycle = StorageVirtualMachineLifecycle'
  { fromStorageVirtualMachineLifecycle ::
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

pattern StorageVirtualMachineLifecycle_CREATED :: StorageVirtualMachineLifecycle
pattern StorageVirtualMachineLifecycle_CREATED = StorageVirtualMachineLifecycle' "CREATED"

pattern StorageVirtualMachineLifecycle_CREATING :: StorageVirtualMachineLifecycle
pattern StorageVirtualMachineLifecycle_CREATING = StorageVirtualMachineLifecycle' "CREATING"

pattern StorageVirtualMachineLifecycle_DELETING :: StorageVirtualMachineLifecycle
pattern StorageVirtualMachineLifecycle_DELETING = StorageVirtualMachineLifecycle' "DELETING"

pattern StorageVirtualMachineLifecycle_FAILED :: StorageVirtualMachineLifecycle
pattern StorageVirtualMachineLifecycle_FAILED = StorageVirtualMachineLifecycle' "FAILED"

pattern StorageVirtualMachineLifecycle_MISCONFIGURED :: StorageVirtualMachineLifecycle
pattern StorageVirtualMachineLifecycle_MISCONFIGURED = StorageVirtualMachineLifecycle' "MISCONFIGURED"

pattern StorageVirtualMachineLifecycle_PENDING :: StorageVirtualMachineLifecycle
pattern StorageVirtualMachineLifecycle_PENDING = StorageVirtualMachineLifecycle' "PENDING"

{-# COMPLETE
  StorageVirtualMachineLifecycle_CREATED,
  StorageVirtualMachineLifecycle_CREATING,
  StorageVirtualMachineLifecycle_DELETING,
  StorageVirtualMachineLifecycle_FAILED,
  StorageVirtualMachineLifecycle_MISCONFIGURED,
  StorageVirtualMachineLifecycle_PENDING,
  StorageVirtualMachineLifecycle'
  #-}

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
-- Module      : Amazonka.SMS.Types.VmManagerType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.VmManagerType
  ( VmManagerType
      ( ..,
        VmManagerType_HYPERV_MANAGER,
        VmManagerType_SCVMM,
        VmManagerType_VSPHERE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VmManagerType = VmManagerType'
  { fromVmManagerType ::
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

pattern VmManagerType_HYPERV_MANAGER :: VmManagerType
pattern VmManagerType_HYPERV_MANAGER = VmManagerType' "HYPERV-MANAGER"

pattern VmManagerType_SCVMM :: VmManagerType
pattern VmManagerType_SCVMM = VmManagerType' "SCVMM"

pattern VmManagerType_VSPHERE :: VmManagerType
pattern VmManagerType_VSPHERE = VmManagerType' "VSPHERE"

{-# COMPLETE
  VmManagerType_HYPERV_MANAGER,
  VmManagerType_SCVMM,
  VmManagerType_VSPHERE,
  VmManagerType'
  #-}

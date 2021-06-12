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
-- Module      : Network.AWS.SMS.Types.VmManagerType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.VmManagerType
  ( VmManagerType
      ( ..,
        VmManagerType_HYPERV_MANAGER,
        VmManagerType_SCVMM,
        VmManagerType_VSPHERE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype VmManagerType = VmManagerType'
  { fromVmManagerType ::
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

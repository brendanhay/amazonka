-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.VMManagerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.VMManagerType
  ( VMManagerType
      ( VMManagerType',
        HypervManager,
        Scvmm,
        Vsphere
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype VMManagerType = VMManagerType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern HypervManager :: VMManagerType
pattern HypervManager = VMManagerType' "HYPERV-MANAGER"

pattern Scvmm :: VMManagerType
pattern Scvmm = VMManagerType' "SCVMM"

pattern Vsphere :: VMManagerType
pattern Vsphere = VMManagerType' "VSPHERE"

{-# COMPLETE
  HypervManager,
  Scvmm,
  Vsphere,
  VMManagerType'
  #-}

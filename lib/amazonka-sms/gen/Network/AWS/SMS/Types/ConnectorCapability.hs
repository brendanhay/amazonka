{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ConnectorCapability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ConnectorCapability
  ( ConnectorCapability
      ( ConnectorCapability',
        CCVsphere,
        CCScvmm,
        CCHypervManager,
        CCSnapshotBatching,
        CCSmsOptimized
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ConnectorCapability = ConnectorCapability' Lude.Text
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

pattern CCVsphere :: ConnectorCapability
pattern CCVsphere = ConnectorCapability' "VSPHERE"

pattern CCScvmm :: ConnectorCapability
pattern CCScvmm = ConnectorCapability' "SCVMM"

pattern CCHypervManager :: ConnectorCapability
pattern CCHypervManager = ConnectorCapability' "HYPERV-MANAGER"

pattern CCSnapshotBatching :: ConnectorCapability
pattern CCSnapshotBatching = ConnectorCapability' "SNAPSHOT_BATCHING"

pattern CCSmsOptimized :: ConnectorCapability
pattern CCSmsOptimized = ConnectorCapability' "SMS_OPTIMIZED"

{-# COMPLETE
  CCVsphere,
  CCScvmm,
  CCHypervManager,
  CCSnapshotBatching,
  CCSmsOptimized,
  ConnectorCapability'
  #-}

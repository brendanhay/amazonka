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
-- Module      : Network.AWS.SMS.Types.ConnectorCapability
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ConnectorCapability
  ( ConnectorCapability
      ( ..,
        ConnectorCapability_HYPERV_MANAGER,
        ConnectorCapability_SCVMM,
        ConnectorCapability_SMS_OPTIMIZED,
        ConnectorCapability_SNAPSHOT_BATCHING,
        ConnectorCapability_VSPHERE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ConnectorCapability = ConnectorCapability'
  { fromConnectorCapability ::
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

pattern ConnectorCapability_HYPERV_MANAGER :: ConnectorCapability
pattern ConnectorCapability_HYPERV_MANAGER = ConnectorCapability' "HYPERV-MANAGER"

pattern ConnectorCapability_SCVMM :: ConnectorCapability
pattern ConnectorCapability_SCVMM = ConnectorCapability' "SCVMM"

pattern ConnectorCapability_SMS_OPTIMIZED :: ConnectorCapability
pattern ConnectorCapability_SMS_OPTIMIZED = ConnectorCapability' "SMS_OPTIMIZED"

pattern ConnectorCapability_SNAPSHOT_BATCHING :: ConnectorCapability
pattern ConnectorCapability_SNAPSHOT_BATCHING = ConnectorCapability' "SNAPSHOT_BATCHING"

pattern ConnectorCapability_VSPHERE :: ConnectorCapability
pattern ConnectorCapability_VSPHERE = ConnectorCapability' "VSPHERE"

{-# COMPLETE
  ConnectorCapability_HYPERV_MANAGER,
  ConnectorCapability_SCVMM,
  ConnectorCapability_SMS_OPTIMIZED,
  ConnectorCapability_SNAPSHOT_BATCHING,
  ConnectorCapability_VSPHERE,
  ConnectorCapability'
  #-}

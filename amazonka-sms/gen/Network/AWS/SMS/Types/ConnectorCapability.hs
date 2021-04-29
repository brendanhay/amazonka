{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype ConnectorCapability = ConnectorCapability'
  { fromConnectorCapability ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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

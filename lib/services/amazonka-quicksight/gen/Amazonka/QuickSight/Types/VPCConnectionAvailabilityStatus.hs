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
-- Module      : Amazonka.QuickSight.Types.VPCConnectionAvailabilityStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.VPCConnectionAvailabilityStatus
  ( VPCConnectionAvailabilityStatus
      ( ..,
        VPCConnectionAvailabilityStatus_AVAILABLE,
        VPCConnectionAvailabilityStatus_PARTIALLY_AVAILABLE,
        VPCConnectionAvailabilityStatus_UNAVAILABLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VPCConnectionAvailabilityStatus = VPCConnectionAvailabilityStatus'
  { fromVPCConnectionAvailabilityStatus ::
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

pattern VPCConnectionAvailabilityStatus_AVAILABLE :: VPCConnectionAvailabilityStatus
pattern VPCConnectionAvailabilityStatus_AVAILABLE = VPCConnectionAvailabilityStatus' "AVAILABLE"

pattern VPCConnectionAvailabilityStatus_PARTIALLY_AVAILABLE :: VPCConnectionAvailabilityStatus
pattern VPCConnectionAvailabilityStatus_PARTIALLY_AVAILABLE = VPCConnectionAvailabilityStatus' "PARTIALLY_AVAILABLE"

pattern VPCConnectionAvailabilityStatus_UNAVAILABLE :: VPCConnectionAvailabilityStatus
pattern VPCConnectionAvailabilityStatus_UNAVAILABLE = VPCConnectionAvailabilityStatus' "UNAVAILABLE"

{-# COMPLETE
  VPCConnectionAvailabilityStatus_AVAILABLE,
  VPCConnectionAvailabilityStatus_PARTIALLY_AVAILABLE,
  VPCConnectionAvailabilityStatus_UNAVAILABLE,
  VPCConnectionAvailabilityStatus'
  #-}

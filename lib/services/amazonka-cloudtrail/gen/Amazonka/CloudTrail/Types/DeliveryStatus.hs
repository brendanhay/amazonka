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
-- Module      : Amazonka.CloudTrail.Types.DeliveryStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.DeliveryStatus
  ( DeliveryStatus
      ( ..,
        DeliveryStatus_ACCESS_DENIED,
        DeliveryStatus_ACCESS_DENIED_SIGNING_FILE,
        DeliveryStatus_CANCELLED,
        DeliveryStatus_FAILED,
        DeliveryStatus_FAILED_SIGNING_FILE,
        DeliveryStatus_PENDING,
        DeliveryStatus_RESOURCE_NOT_FOUND,
        DeliveryStatus_SUCCESS,
        DeliveryStatus_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DeliveryStatus = DeliveryStatus'
  { fromDeliveryStatus ::
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

pattern DeliveryStatus_ACCESS_DENIED :: DeliveryStatus
pattern DeliveryStatus_ACCESS_DENIED = DeliveryStatus' "ACCESS_DENIED"

pattern DeliveryStatus_ACCESS_DENIED_SIGNING_FILE :: DeliveryStatus
pattern DeliveryStatus_ACCESS_DENIED_SIGNING_FILE = DeliveryStatus' "ACCESS_DENIED_SIGNING_FILE"

pattern DeliveryStatus_CANCELLED :: DeliveryStatus
pattern DeliveryStatus_CANCELLED = DeliveryStatus' "CANCELLED"

pattern DeliveryStatus_FAILED :: DeliveryStatus
pattern DeliveryStatus_FAILED = DeliveryStatus' "FAILED"

pattern DeliveryStatus_FAILED_SIGNING_FILE :: DeliveryStatus
pattern DeliveryStatus_FAILED_SIGNING_FILE = DeliveryStatus' "FAILED_SIGNING_FILE"

pattern DeliveryStatus_PENDING :: DeliveryStatus
pattern DeliveryStatus_PENDING = DeliveryStatus' "PENDING"

pattern DeliveryStatus_RESOURCE_NOT_FOUND :: DeliveryStatus
pattern DeliveryStatus_RESOURCE_NOT_FOUND = DeliveryStatus' "RESOURCE_NOT_FOUND"

pattern DeliveryStatus_SUCCESS :: DeliveryStatus
pattern DeliveryStatus_SUCCESS = DeliveryStatus' "SUCCESS"

pattern DeliveryStatus_UNKNOWN :: DeliveryStatus
pattern DeliveryStatus_UNKNOWN = DeliveryStatus' "UNKNOWN"

{-# COMPLETE
  DeliveryStatus_ACCESS_DENIED,
  DeliveryStatus_ACCESS_DENIED_SIGNING_FILE,
  DeliveryStatus_CANCELLED,
  DeliveryStatus_FAILED,
  DeliveryStatus_FAILED_SIGNING_FILE,
  DeliveryStatus_PENDING,
  DeliveryStatus_RESOURCE_NOT_FOUND,
  DeliveryStatus_SUCCESS,
  DeliveryStatus_UNKNOWN,
  DeliveryStatus'
  #-}

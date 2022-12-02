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
-- Module      : Amazonka.Inspector2.Types.ScanStatusReason
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.ScanStatusReason
  ( ScanStatusReason
      ( ..,
        ScanStatusReason_ACCESS_DENIED,
        ScanStatusReason_EC2_INSTANCE_STOPPED,
        ScanStatusReason_IMAGE_SIZE_EXCEEDED,
        ScanStatusReason_INTERNAL_ERROR,
        ScanStatusReason_NO_INVENTORY,
        ScanStatusReason_NO_RESOURCES_FOUND,
        ScanStatusReason_PENDING_DISABLE,
        ScanStatusReason_PENDING_INITIAL_SCAN,
        ScanStatusReason_RESOURCE_TERMINATED,
        ScanStatusReason_SCAN_ELIGIBILITY_EXPIRED,
        ScanStatusReason_SCAN_FREQUENCY_MANUAL,
        ScanStatusReason_SCAN_FREQUENCY_SCAN_ON_PUSH,
        ScanStatusReason_STALE_INVENTORY,
        ScanStatusReason_SUCCESSFUL,
        ScanStatusReason_UNMANAGED_EC2_INSTANCE,
        ScanStatusReason_UNSUPPORTED_OS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ScanStatusReason = ScanStatusReason'
  { fromScanStatusReason ::
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

pattern ScanStatusReason_ACCESS_DENIED :: ScanStatusReason
pattern ScanStatusReason_ACCESS_DENIED = ScanStatusReason' "ACCESS_DENIED"

pattern ScanStatusReason_EC2_INSTANCE_STOPPED :: ScanStatusReason
pattern ScanStatusReason_EC2_INSTANCE_STOPPED = ScanStatusReason' "EC2_INSTANCE_STOPPED"

pattern ScanStatusReason_IMAGE_SIZE_EXCEEDED :: ScanStatusReason
pattern ScanStatusReason_IMAGE_SIZE_EXCEEDED = ScanStatusReason' "IMAGE_SIZE_EXCEEDED"

pattern ScanStatusReason_INTERNAL_ERROR :: ScanStatusReason
pattern ScanStatusReason_INTERNAL_ERROR = ScanStatusReason' "INTERNAL_ERROR"

pattern ScanStatusReason_NO_INVENTORY :: ScanStatusReason
pattern ScanStatusReason_NO_INVENTORY = ScanStatusReason' "NO_INVENTORY"

pattern ScanStatusReason_NO_RESOURCES_FOUND :: ScanStatusReason
pattern ScanStatusReason_NO_RESOURCES_FOUND = ScanStatusReason' "NO_RESOURCES_FOUND"

pattern ScanStatusReason_PENDING_DISABLE :: ScanStatusReason
pattern ScanStatusReason_PENDING_DISABLE = ScanStatusReason' "PENDING_DISABLE"

pattern ScanStatusReason_PENDING_INITIAL_SCAN :: ScanStatusReason
pattern ScanStatusReason_PENDING_INITIAL_SCAN = ScanStatusReason' "PENDING_INITIAL_SCAN"

pattern ScanStatusReason_RESOURCE_TERMINATED :: ScanStatusReason
pattern ScanStatusReason_RESOURCE_TERMINATED = ScanStatusReason' "RESOURCE_TERMINATED"

pattern ScanStatusReason_SCAN_ELIGIBILITY_EXPIRED :: ScanStatusReason
pattern ScanStatusReason_SCAN_ELIGIBILITY_EXPIRED = ScanStatusReason' "SCAN_ELIGIBILITY_EXPIRED"

pattern ScanStatusReason_SCAN_FREQUENCY_MANUAL :: ScanStatusReason
pattern ScanStatusReason_SCAN_FREQUENCY_MANUAL = ScanStatusReason' "SCAN_FREQUENCY_MANUAL"

pattern ScanStatusReason_SCAN_FREQUENCY_SCAN_ON_PUSH :: ScanStatusReason
pattern ScanStatusReason_SCAN_FREQUENCY_SCAN_ON_PUSH = ScanStatusReason' "SCAN_FREQUENCY_SCAN_ON_PUSH"

pattern ScanStatusReason_STALE_INVENTORY :: ScanStatusReason
pattern ScanStatusReason_STALE_INVENTORY = ScanStatusReason' "STALE_INVENTORY"

pattern ScanStatusReason_SUCCESSFUL :: ScanStatusReason
pattern ScanStatusReason_SUCCESSFUL = ScanStatusReason' "SUCCESSFUL"

pattern ScanStatusReason_UNMANAGED_EC2_INSTANCE :: ScanStatusReason
pattern ScanStatusReason_UNMANAGED_EC2_INSTANCE = ScanStatusReason' "UNMANAGED_EC2_INSTANCE"

pattern ScanStatusReason_UNSUPPORTED_OS :: ScanStatusReason
pattern ScanStatusReason_UNSUPPORTED_OS = ScanStatusReason' "UNSUPPORTED_OS"

{-# COMPLETE
  ScanStatusReason_ACCESS_DENIED,
  ScanStatusReason_EC2_INSTANCE_STOPPED,
  ScanStatusReason_IMAGE_SIZE_EXCEEDED,
  ScanStatusReason_INTERNAL_ERROR,
  ScanStatusReason_NO_INVENTORY,
  ScanStatusReason_NO_RESOURCES_FOUND,
  ScanStatusReason_PENDING_DISABLE,
  ScanStatusReason_PENDING_INITIAL_SCAN,
  ScanStatusReason_RESOURCE_TERMINATED,
  ScanStatusReason_SCAN_ELIGIBILITY_EXPIRED,
  ScanStatusReason_SCAN_FREQUENCY_MANUAL,
  ScanStatusReason_SCAN_FREQUENCY_SCAN_ON_PUSH,
  ScanStatusReason_STALE_INVENTORY,
  ScanStatusReason_SUCCESSFUL,
  ScanStatusReason_UNMANAGED_EC2_INSTANCE,
  ScanStatusReason_UNSUPPORTED_OS,
  ScanStatusReason'
  #-}

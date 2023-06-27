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
-- Module      : Amazonka.QuickSight.Types.NetworkInterfaceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NetworkInterfaceStatus
  ( NetworkInterfaceStatus
      ( ..,
        NetworkInterfaceStatus_ATTACHMENT_FAILED_ROLLBACK_FAILED,
        NetworkInterfaceStatus_AVAILABLE,
        NetworkInterfaceStatus_CREATING,
        NetworkInterfaceStatus_CREATION_FAILED,
        NetworkInterfaceStatus_DELETED,
        NetworkInterfaceStatus_DELETING,
        NetworkInterfaceStatus_DELETION_FAILED,
        NetworkInterfaceStatus_DELETION_SCHEDULED,
        NetworkInterfaceStatus_UPDATE_FAILED,
        NetworkInterfaceStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NetworkInterfaceStatus = NetworkInterfaceStatus'
  { fromNetworkInterfaceStatus ::
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

pattern NetworkInterfaceStatus_ATTACHMENT_FAILED_ROLLBACK_FAILED :: NetworkInterfaceStatus
pattern NetworkInterfaceStatus_ATTACHMENT_FAILED_ROLLBACK_FAILED = NetworkInterfaceStatus' "ATTACHMENT_FAILED_ROLLBACK_FAILED"

pattern NetworkInterfaceStatus_AVAILABLE :: NetworkInterfaceStatus
pattern NetworkInterfaceStatus_AVAILABLE = NetworkInterfaceStatus' "AVAILABLE"

pattern NetworkInterfaceStatus_CREATING :: NetworkInterfaceStatus
pattern NetworkInterfaceStatus_CREATING = NetworkInterfaceStatus' "CREATING"

pattern NetworkInterfaceStatus_CREATION_FAILED :: NetworkInterfaceStatus
pattern NetworkInterfaceStatus_CREATION_FAILED = NetworkInterfaceStatus' "CREATION_FAILED"

pattern NetworkInterfaceStatus_DELETED :: NetworkInterfaceStatus
pattern NetworkInterfaceStatus_DELETED = NetworkInterfaceStatus' "DELETED"

pattern NetworkInterfaceStatus_DELETING :: NetworkInterfaceStatus
pattern NetworkInterfaceStatus_DELETING = NetworkInterfaceStatus' "DELETING"

pattern NetworkInterfaceStatus_DELETION_FAILED :: NetworkInterfaceStatus
pattern NetworkInterfaceStatus_DELETION_FAILED = NetworkInterfaceStatus' "DELETION_FAILED"

pattern NetworkInterfaceStatus_DELETION_SCHEDULED :: NetworkInterfaceStatus
pattern NetworkInterfaceStatus_DELETION_SCHEDULED = NetworkInterfaceStatus' "DELETION_SCHEDULED"

pattern NetworkInterfaceStatus_UPDATE_FAILED :: NetworkInterfaceStatus
pattern NetworkInterfaceStatus_UPDATE_FAILED = NetworkInterfaceStatus' "UPDATE_FAILED"

pattern NetworkInterfaceStatus_UPDATING :: NetworkInterfaceStatus
pattern NetworkInterfaceStatus_UPDATING = NetworkInterfaceStatus' "UPDATING"

{-# COMPLETE
  NetworkInterfaceStatus_ATTACHMENT_FAILED_ROLLBACK_FAILED,
  NetworkInterfaceStatus_AVAILABLE,
  NetworkInterfaceStatus_CREATING,
  NetworkInterfaceStatus_CREATION_FAILED,
  NetworkInterfaceStatus_DELETED,
  NetworkInterfaceStatus_DELETING,
  NetworkInterfaceStatus_DELETION_FAILED,
  NetworkInterfaceStatus_DELETION_SCHEDULED,
  NetworkInterfaceStatus_UPDATE_FAILED,
  NetworkInterfaceStatus_UPDATING,
  NetworkInterfaceStatus'
  #-}

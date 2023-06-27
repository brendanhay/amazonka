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
-- Module      : Amazonka.FinSpace.Types.DnsStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.DnsStatus
  ( DnsStatus
      ( ..,
        DnsStatus_FAILED_UPDATE,
        DnsStatus_NONE,
        DnsStatus_SUCCESSFULLY_UPDATED,
        DnsStatus_UPDATE_REQUESTED,
        DnsStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DnsStatus = DnsStatus'
  { fromDnsStatus ::
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

pattern DnsStatus_FAILED_UPDATE :: DnsStatus
pattern DnsStatus_FAILED_UPDATE = DnsStatus' "FAILED_UPDATE"

pattern DnsStatus_NONE :: DnsStatus
pattern DnsStatus_NONE = DnsStatus' "NONE"

pattern DnsStatus_SUCCESSFULLY_UPDATED :: DnsStatus
pattern DnsStatus_SUCCESSFULLY_UPDATED = DnsStatus' "SUCCESSFULLY_UPDATED"

pattern DnsStatus_UPDATE_REQUESTED :: DnsStatus
pattern DnsStatus_UPDATE_REQUESTED = DnsStatus' "UPDATE_REQUESTED"

pattern DnsStatus_UPDATING :: DnsStatus
pattern DnsStatus_UPDATING = DnsStatus' "UPDATING"

{-# COMPLETE
  DnsStatus_FAILED_UPDATE,
  DnsStatus_NONE,
  DnsStatus_SUCCESSFULLY_UPDATED,
  DnsStatus_UPDATE_REQUESTED,
  DnsStatus_UPDATING,
  DnsStatus'
  #-}

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
-- Module      : Amazonka.NetworkFirewall.Types.PerObjectSyncStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.PerObjectSyncStatus
  ( PerObjectSyncStatus
      ( ..,
        PerObjectSyncStatus_CAPACITY_CONSTRAINED,
        PerObjectSyncStatus_IN_SYNC,
        PerObjectSyncStatus_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PerObjectSyncStatus = PerObjectSyncStatus'
  { fromPerObjectSyncStatus ::
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

pattern PerObjectSyncStatus_CAPACITY_CONSTRAINED :: PerObjectSyncStatus
pattern PerObjectSyncStatus_CAPACITY_CONSTRAINED = PerObjectSyncStatus' "CAPACITY_CONSTRAINED"

pattern PerObjectSyncStatus_IN_SYNC :: PerObjectSyncStatus
pattern PerObjectSyncStatus_IN_SYNC = PerObjectSyncStatus' "IN_SYNC"

pattern PerObjectSyncStatus_PENDING :: PerObjectSyncStatus
pattern PerObjectSyncStatus_PENDING = PerObjectSyncStatus' "PENDING"

{-# COMPLETE
  PerObjectSyncStatus_CAPACITY_CONSTRAINED,
  PerObjectSyncStatus_IN_SYNC,
  PerObjectSyncStatus_PENDING,
  PerObjectSyncStatus'
  #-}

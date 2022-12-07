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
-- Module      : Amazonka.Proton.Types.ResourceSyncStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.ResourceSyncStatus
  ( ResourceSyncStatus
      ( ..,
        ResourceSyncStatus_FAILED,
        ResourceSyncStatus_INITIATED,
        ResourceSyncStatus_IN_PROGRESS,
        ResourceSyncStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceSyncStatus = ResourceSyncStatus'
  { fromResourceSyncStatus ::
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

pattern ResourceSyncStatus_FAILED :: ResourceSyncStatus
pattern ResourceSyncStatus_FAILED = ResourceSyncStatus' "FAILED"

pattern ResourceSyncStatus_INITIATED :: ResourceSyncStatus
pattern ResourceSyncStatus_INITIATED = ResourceSyncStatus' "INITIATED"

pattern ResourceSyncStatus_IN_PROGRESS :: ResourceSyncStatus
pattern ResourceSyncStatus_IN_PROGRESS = ResourceSyncStatus' "IN_PROGRESS"

pattern ResourceSyncStatus_SUCCEEDED :: ResourceSyncStatus
pattern ResourceSyncStatus_SUCCEEDED = ResourceSyncStatus' "SUCCEEDED"

{-# COMPLETE
  ResourceSyncStatus_FAILED,
  ResourceSyncStatus_INITIATED,
  ResourceSyncStatus_IN_PROGRESS,
  ResourceSyncStatus_SUCCEEDED,
  ResourceSyncStatus'
  #-}

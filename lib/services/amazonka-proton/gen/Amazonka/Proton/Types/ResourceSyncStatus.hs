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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
import qualified Amazonka.Prelude as Prelude

newtype ResourceSyncStatus = ResourceSyncStatus'
  { fromResourceSyncStatus ::
      Core.Text
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

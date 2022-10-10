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
-- Module      : Amazonka.Translate.Types.ParallelDataStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Types.ParallelDataStatus
  ( ParallelDataStatus
      ( ..,
        ParallelDataStatus_ACTIVE,
        ParallelDataStatus_CREATING,
        ParallelDataStatus_DELETING,
        ParallelDataStatus_FAILED,
        ParallelDataStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ParallelDataStatus = ParallelDataStatus'
  { fromParallelDataStatus ::
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

pattern ParallelDataStatus_ACTIVE :: ParallelDataStatus
pattern ParallelDataStatus_ACTIVE = ParallelDataStatus' "ACTIVE"

pattern ParallelDataStatus_CREATING :: ParallelDataStatus
pattern ParallelDataStatus_CREATING = ParallelDataStatus' "CREATING"

pattern ParallelDataStatus_DELETING :: ParallelDataStatus
pattern ParallelDataStatus_DELETING = ParallelDataStatus' "DELETING"

pattern ParallelDataStatus_FAILED :: ParallelDataStatus
pattern ParallelDataStatus_FAILED = ParallelDataStatus' "FAILED"

pattern ParallelDataStatus_UPDATING :: ParallelDataStatus
pattern ParallelDataStatus_UPDATING = ParallelDataStatus' "UPDATING"

{-# COMPLETE
  ParallelDataStatus_ACTIVE,
  ParallelDataStatus_CREATING,
  ParallelDataStatus_DELETING,
  ParallelDataStatus_FAILED,
  ParallelDataStatus_UPDATING,
  ParallelDataStatus'
  #-}

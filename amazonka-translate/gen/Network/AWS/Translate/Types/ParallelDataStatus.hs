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
-- Module      : Network.AWS.Translate.Types.ParallelDataStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.ParallelDataStatus
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

import qualified Network.AWS.Core as Core

newtype ParallelDataStatus = ParallelDataStatus'
  { fromParallelDataStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

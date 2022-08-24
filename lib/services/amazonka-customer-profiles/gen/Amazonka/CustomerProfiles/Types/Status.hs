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
-- Module      : Amazonka.CustomerProfiles.Types.Status
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.Status
  ( Status
      ( ..,
        Status_CANCELLED,
        Status_COMPLETE,
        Status_FAILED,
        Status_IN_PROGRESS,
        Status_NOT_STARTED,
        Status_RETRY,
        Status_SPLIT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype Status = Status' {fromStatus :: Core.Text}
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

pattern Status_CANCELLED :: Status
pattern Status_CANCELLED = Status' "CANCELLED"

pattern Status_COMPLETE :: Status
pattern Status_COMPLETE = Status' "COMPLETE"

pattern Status_FAILED :: Status
pattern Status_FAILED = Status' "FAILED"

pattern Status_IN_PROGRESS :: Status
pattern Status_IN_PROGRESS = Status' "IN_PROGRESS"

pattern Status_NOT_STARTED :: Status
pattern Status_NOT_STARTED = Status' "NOT_STARTED"

pattern Status_RETRY :: Status
pattern Status_RETRY = Status' "RETRY"

pattern Status_SPLIT :: Status
pattern Status_SPLIT = Status' "SPLIT"

{-# COMPLETE
  Status_CANCELLED,
  Status_COMPLETE,
  Status_FAILED,
  Status_IN_PROGRESS,
  Status_NOT_STARTED,
  Status_RETRY,
  Status_SPLIT,
  Status'
  #-}

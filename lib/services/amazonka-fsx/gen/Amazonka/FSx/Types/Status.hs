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
-- Module      : Amazonka.FSx.Types.Status
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.Status
  ( Status
      ( ..,
        Status_COMPLETED,
        Status_FAILED,
        Status_IN_PROGRESS,
        Status_PENDING,
        Status_UPDATED_OPTIMIZING
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

pattern Status_COMPLETED :: Status
pattern Status_COMPLETED = Status' "COMPLETED"

pattern Status_FAILED :: Status
pattern Status_FAILED = Status' "FAILED"

pattern Status_IN_PROGRESS :: Status
pattern Status_IN_PROGRESS = Status' "IN_PROGRESS"

pattern Status_PENDING :: Status
pattern Status_PENDING = Status' "PENDING"

pattern Status_UPDATED_OPTIMIZING :: Status
pattern Status_UPDATED_OPTIMIZING = Status' "UPDATED_OPTIMIZING"

{-# COMPLETE
  Status_COMPLETED,
  Status_FAILED,
  Status_IN_PROGRESS,
  Status_PENDING,
  Status_UPDATED_OPTIMIZING,
  Status'
  #-}

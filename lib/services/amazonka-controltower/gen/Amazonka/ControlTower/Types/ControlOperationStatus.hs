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
-- Module      : Amazonka.ControlTower.Types.ControlOperationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ControlTower.Types.ControlOperationStatus
  ( ControlOperationStatus
      ( ..,
        ControlOperationStatus_FAILED,
        ControlOperationStatus_IN_PROGRESS,
        ControlOperationStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ControlOperationStatus = ControlOperationStatus'
  { fromControlOperationStatus ::
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

pattern ControlOperationStatus_FAILED :: ControlOperationStatus
pattern ControlOperationStatus_FAILED = ControlOperationStatus' "FAILED"

pattern ControlOperationStatus_IN_PROGRESS :: ControlOperationStatus
pattern ControlOperationStatus_IN_PROGRESS = ControlOperationStatus' "IN_PROGRESS"

pattern ControlOperationStatus_SUCCEEDED :: ControlOperationStatus
pattern ControlOperationStatus_SUCCEEDED = ControlOperationStatus' "SUCCEEDED"

{-# COMPLETE
  ControlOperationStatus_FAILED,
  ControlOperationStatus_IN_PROGRESS,
  ControlOperationStatus_SUCCEEDED,
  ControlOperationStatus'
  #-}

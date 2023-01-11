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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ControlOperationStatus = ControlOperationStatus'
  { fromControlOperationStatus ::
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

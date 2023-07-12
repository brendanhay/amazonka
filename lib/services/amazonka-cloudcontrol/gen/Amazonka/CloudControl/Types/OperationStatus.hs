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
-- Module      : Amazonka.CloudControl.Types.OperationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudControl.Types.OperationStatus
  ( OperationStatus
      ( ..,
        OperationStatus_CANCEL_COMPLETE,
        OperationStatus_CANCEL_IN_PROGRESS,
        OperationStatus_FAILED,
        OperationStatus_IN_PROGRESS,
        OperationStatus_PENDING,
        OperationStatus_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OperationStatus = OperationStatus'
  { fromOperationStatus ::
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

pattern OperationStatus_CANCEL_COMPLETE :: OperationStatus
pattern OperationStatus_CANCEL_COMPLETE = OperationStatus' "CANCEL_COMPLETE"

pattern OperationStatus_CANCEL_IN_PROGRESS :: OperationStatus
pattern OperationStatus_CANCEL_IN_PROGRESS = OperationStatus' "CANCEL_IN_PROGRESS"

pattern OperationStatus_FAILED :: OperationStatus
pattern OperationStatus_FAILED = OperationStatus' "FAILED"

pattern OperationStatus_IN_PROGRESS :: OperationStatus
pattern OperationStatus_IN_PROGRESS = OperationStatus' "IN_PROGRESS"

pattern OperationStatus_PENDING :: OperationStatus
pattern OperationStatus_PENDING = OperationStatus' "PENDING"

pattern OperationStatus_SUCCESS :: OperationStatus
pattern OperationStatus_SUCCESS = OperationStatus' "SUCCESS"

{-# COMPLETE
  OperationStatus_CANCEL_COMPLETE,
  OperationStatus_CANCEL_IN_PROGRESS,
  OperationStatus_FAILED,
  OperationStatus_IN_PROGRESS,
  OperationStatus_PENDING,
  OperationStatus_SUCCESS,
  OperationStatus'
  #-}

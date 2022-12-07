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
-- Module      : Amazonka.SMS.Types.ValidationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.ValidationStatus
  ( ValidationStatus
      ( ..,
        ValidationStatus_FAILED,
        ValidationStatus_IN_PROGRESS,
        ValidationStatus_PENDING,
        ValidationStatus_READY_FOR_VALIDATION,
        ValidationStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ValidationStatus = ValidationStatus'
  { fromValidationStatus ::
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

pattern ValidationStatus_FAILED :: ValidationStatus
pattern ValidationStatus_FAILED = ValidationStatus' "FAILED"

pattern ValidationStatus_IN_PROGRESS :: ValidationStatus
pattern ValidationStatus_IN_PROGRESS = ValidationStatus' "IN_PROGRESS"

pattern ValidationStatus_PENDING :: ValidationStatus
pattern ValidationStatus_PENDING = ValidationStatus' "PENDING"

pattern ValidationStatus_READY_FOR_VALIDATION :: ValidationStatus
pattern ValidationStatus_READY_FOR_VALIDATION = ValidationStatus' "READY_FOR_VALIDATION"

pattern ValidationStatus_SUCCEEDED :: ValidationStatus
pattern ValidationStatus_SUCCEEDED = ValidationStatus' "SUCCEEDED"

{-# COMPLETE
  ValidationStatus_FAILED,
  ValidationStatus_IN_PROGRESS,
  ValidationStatus_PENDING,
  ValidationStatus_READY_FOR_VALIDATION,
  ValidationStatus_SUCCEEDED,
  ValidationStatus'
  #-}

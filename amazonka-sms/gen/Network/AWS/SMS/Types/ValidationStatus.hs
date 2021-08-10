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
-- Module      : Network.AWS.SMS.Types.ValidationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ValidationStatus
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ValidationStatus = ValidationStatus'
  { fromValidationStatus ::
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

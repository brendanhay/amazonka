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
-- Module      : Amazonka.VoiceId.Types.FraudsterRegistrationJobStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.FraudsterRegistrationJobStatus
  ( FraudsterRegistrationJobStatus
      ( ..,
        FraudsterRegistrationJobStatus_COMPLETED,
        FraudsterRegistrationJobStatus_COMPLETED_WITH_ERRORS,
        FraudsterRegistrationJobStatus_FAILED,
        FraudsterRegistrationJobStatus_IN_PROGRESS,
        FraudsterRegistrationJobStatus_SUBMITTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype FraudsterRegistrationJobStatus = FraudsterRegistrationJobStatus'
  { fromFraudsterRegistrationJobStatus ::
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

pattern FraudsterRegistrationJobStatus_COMPLETED :: FraudsterRegistrationJobStatus
pattern FraudsterRegistrationJobStatus_COMPLETED = FraudsterRegistrationJobStatus' "COMPLETED"

pattern FraudsterRegistrationJobStatus_COMPLETED_WITH_ERRORS :: FraudsterRegistrationJobStatus
pattern FraudsterRegistrationJobStatus_COMPLETED_WITH_ERRORS = FraudsterRegistrationJobStatus' "COMPLETED_WITH_ERRORS"

pattern FraudsterRegistrationJobStatus_FAILED :: FraudsterRegistrationJobStatus
pattern FraudsterRegistrationJobStatus_FAILED = FraudsterRegistrationJobStatus' "FAILED"

pattern FraudsterRegistrationJobStatus_IN_PROGRESS :: FraudsterRegistrationJobStatus
pattern FraudsterRegistrationJobStatus_IN_PROGRESS = FraudsterRegistrationJobStatus' "IN_PROGRESS"

pattern FraudsterRegistrationJobStatus_SUBMITTED :: FraudsterRegistrationJobStatus
pattern FraudsterRegistrationJobStatus_SUBMITTED = FraudsterRegistrationJobStatus' "SUBMITTED"

{-# COMPLETE
  FraudsterRegistrationJobStatus_COMPLETED,
  FraudsterRegistrationJobStatus_COMPLETED_WITH_ERRORS,
  FraudsterRegistrationJobStatus_FAILED,
  FraudsterRegistrationJobStatus_IN_PROGRESS,
  FraudsterRegistrationJobStatus_SUBMITTED,
  FraudsterRegistrationJobStatus'
  #-}

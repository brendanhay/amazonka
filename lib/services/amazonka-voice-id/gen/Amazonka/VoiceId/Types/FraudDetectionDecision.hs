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
-- Module      : Amazonka.VoiceId.Types.FraudDetectionDecision
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.FraudDetectionDecision
  ( FraudDetectionDecision
      ( ..,
        FraudDetectionDecision_HIGH_RISK,
        FraudDetectionDecision_LOW_RISK,
        FraudDetectionDecision_NOT_ENOUGH_SPEECH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FraudDetectionDecision = FraudDetectionDecision'
  { fromFraudDetectionDecision ::
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

pattern FraudDetectionDecision_HIGH_RISK :: FraudDetectionDecision
pattern FraudDetectionDecision_HIGH_RISK = FraudDetectionDecision' "HIGH_RISK"

pattern FraudDetectionDecision_LOW_RISK :: FraudDetectionDecision
pattern FraudDetectionDecision_LOW_RISK = FraudDetectionDecision' "LOW_RISK"

pattern FraudDetectionDecision_NOT_ENOUGH_SPEECH :: FraudDetectionDecision
pattern FraudDetectionDecision_NOT_ENOUGH_SPEECH = FraudDetectionDecision' "NOT_ENOUGH_SPEECH"

{-# COMPLETE
  FraudDetectionDecision_HIGH_RISK,
  FraudDetectionDecision_LOW_RISK,
  FraudDetectionDecision_NOT_ENOUGH_SPEECH,
  FraudDetectionDecision'
  #-}

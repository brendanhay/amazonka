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
-- Module      : Network.AWS.VoiceId.Types.FraudDetectionDecision
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.VoiceId.Types.FraudDetectionDecision
  ( FraudDetectionDecision
      ( ..,
        FraudDetectionDecision_HIGH_RISK,
        FraudDetectionDecision_LOW_RISK,
        FraudDetectionDecision_NOT_ENOUGH_SPEECH
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype FraudDetectionDecision = FraudDetectionDecision'
  { fromFraudDetectionDecision ::
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

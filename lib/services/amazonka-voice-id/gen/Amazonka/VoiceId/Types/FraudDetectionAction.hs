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
-- Module      : Amazonka.VoiceId.Types.FraudDetectionAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.FraudDetectionAction
  ( FraudDetectionAction
      ( ..,
        FraudDetectionAction_FAIL,
        FraudDetectionAction_IGNORE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype FraudDetectionAction = FraudDetectionAction'
  { fromFraudDetectionAction ::
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

pattern FraudDetectionAction_FAIL :: FraudDetectionAction
pattern FraudDetectionAction_FAIL = FraudDetectionAction' "FAIL"

pattern FraudDetectionAction_IGNORE :: FraudDetectionAction
pattern FraudDetectionAction_IGNORE = FraudDetectionAction' "IGNORE"

{-# COMPLETE
  FraudDetectionAction_FAIL,
  FraudDetectionAction_IGNORE,
  FraudDetectionAction'
  #-}

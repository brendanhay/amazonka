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
-- Module      : Amazonka.TNB.Types.OnboardingState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.OnboardingState
  ( OnboardingState
      ( ..,
        OnboardingState_CREATED,
        OnboardingState_ERROR,
        OnboardingState_ONBOARDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OnboardingState = OnboardingState'
  { fromOnboardingState ::
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

pattern OnboardingState_CREATED :: OnboardingState
pattern OnboardingState_CREATED = OnboardingState' "CREATED"

pattern OnboardingState_ERROR :: OnboardingState
pattern OnboardingState_ERROR = OnboardingState' "ERROR"

pattern OnboardingState_ONBOARDED :: OnboardingState
pattern OnboardingState_ONBOARDED = OnboardingState' "ONBOARDED"

{-# COMPLETE
  OnboardingState_CREATED,
  OnboardingState_ERROR,
  OnboardingState_ONBOARDED,
  OnboardingState'
  #-}

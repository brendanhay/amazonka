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
-- Module      : Amazonka.IoTWireless.Types.OnboardStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.OnboardStatus
  ( OnboardStatus
      ( ..,
        OnboardStatus_FAILED,
        OnboardStatus_INITIALIZED,
        OnboardStatus_ONBOARDED,
        OnboardStatus_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OnboardStatus = OnboardStatus'
  { fromOnboardStatus ::
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

pattern OnboardStatus_FAILED :: OnboardStatus
pattern OnboardStatus_FAILED = OnboardStatus' "FAILED"

pattern OnboardStatus_INITIALIZED :: OnboardStatus
pattern OnboardStatus_INITIALIZED = OnboardStatus' "INITIALIZED"

pattern OnboardStatus_ONBOARDED :: OnboardStatus
pattern OnboardStatus_ONBOARDED = OnboardStatus' "ONBOARDED"

pattern OnboardStatus_PENDING :: OnboardStatus
pattern OnboardStatus_PENDING = OnboardStatus' "PENDING"

{-# COMPLETE
  OnboardStatus_FAILED,
  OnboardStatus_INITIALIZED,
  OnboardStatus_ONBOARDED,
  OnboardStatus_PENDING,
  OnboardStatus'
  #-}

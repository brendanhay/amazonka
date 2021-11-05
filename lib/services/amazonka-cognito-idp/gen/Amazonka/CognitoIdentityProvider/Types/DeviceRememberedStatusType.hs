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
-- Module      : Amazonka.CognitoIdentityProvider.Types.DeviceRememberedStatusType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.DeviceRememberedStatusType
  ( DeviceRememberedStatusType
      ( ..,
        DeviceRememberedStatusType_Not_remembered,
        DeviceRememberedStatusType_Remembered
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DeviceRememberedStatusType = DeviceRememberedStatusType'
  { fromDeviceRememberedStatusType ::
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

pattern DeviceRememberedStatusType_Not_remembered :: DeviceRememberedStatusType
pattern DeviceRememberedStatusType_Not_remembered = DeviceRememberedStatusType' "not_remembered"

pattern DeviceRememberedStatusType_Remembered :: DeviceRememberedStatusType
pattern DeviceRememberedStatusType_Remembered = DeviceRememberedStatusType' "remembered"

{-# COMPLETE
  DeviceRememberedStatusType_Not_remembered,
  DeviceRememberedStatusType_Remembered,
  DeviceRememberedStatusType'
  #-}

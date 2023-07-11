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
-- Module      : Amazonka.SMS.Types.AppLaunchConfigurationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.AppLaunchConfigurationStatus
  ( AppLaunchConfigurationStatus
      ( ..,
        AppLaunchConfigurationStatus_CONFIGURED,
        AppLaunchConfigurationStatus_NOT_CONFIGURED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AppLaunchConfigurationStatus = AppLaunchConfigurationStatus'
  { fromAppLaunchConfigurationStatus ::
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

pattern AppLaunchConfigurationStatus_CONFIGURED :: AppLaunchConfigurationStatus
pattern AppLaunchConfigurationStatus_CONFIGURED = AppLaunchConfigurationStatus' "CONFIGURED"

pattern AppLaunchConfigurationStatus_NOT_CONFIGURED :: AppLaunchConfigurationStatus
pattern AppLaunchConfigurationStatus_NOT_CONFIGURED = AppLaunchConfigurationStatus' "NOT_CONFIGURED"

{-# COMPLETE
  AppLaunchConfigurationStatus_CONFIGURED,
  AppLaunchConfigurationStatus_NOT_CONFIGURED,
  AppLaunchConfigurationStatus'
  #-}

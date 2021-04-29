{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.AppLaunchConfigurationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.AppLaunchConfigurationStatus
  ( AppLaunchConfigurationStatus
      ( ..,
        AppLaunchConfigurationStatus_CONFIGURED,
        AppLaunchConfigurationStatus_NOT_CONFIGURED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AppLaunchConfigurationStatus = AppLaunchConfigurationStatus'
  { fromAppLaunchConfigurationStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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

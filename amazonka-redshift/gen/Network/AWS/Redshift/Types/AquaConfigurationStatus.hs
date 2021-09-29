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
-- Module      : Network.AWS.Redshift.Types.AquaConfigurationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.AquaConfigurationStatus
  ( AquaConfigurationStatus
      ( ..,
        AquaConfigurationStatus_Auto,
        AquaConfigurationStatus_Disabled,
        AquaConfigurationStatus_Enabled
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

newtype AquaConfigurationStatus = AquaConfigurationStatus'
  { fromAquaConfigurationStatus ::
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

pattern AquaConfigurationStatus_Auto :: AquaConfigurationStatus
pattern AquaConfigurationStatus_Auto = AquaConfigurationStatus' "auto"

pattern AquaConfigurationStatus_Disabled :: AquaConfigurationStatus
pattern AquaConfigurationStatus_Disabled = AquaConfigurationStatus' "disabled"

pattern AquaConfigurationStatus_Enabled :: AquaConfigurationStatus
pattern AquaConfigurationStatus_Enabled = AquaConfigurationStatus' "enabled"

{-# COMPLETE
  AquaConfigurationStatus_Auto,
  AquaConfigurationStatus_Disabled,
  AquaConfigurationStatus_Enabled,
  AquaConfigurationStatus'
  #-}

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
-- Module      : Network.AWS.ElastiCache.Types.AutomaticFailoverStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.AutomaticFailoverStatus
  ( AutomaticFailoverStatus
      ( ..,
        AutomaticFailoverStatus_Disabled,
        AutomaticFailoverStatus_Disabling,
        AutomaticFailoverStatus_Enabled,
        AutomaticFailoverStatus_Enabling
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AutomaticFailoverStatus = AutomaticFailoverStatus'
  { fromAutomaticFailoverStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern AutomaticFailoverStatus_Disabled :: AutomaticFailoverStatus
pattern AutomaticFailoverStatus_Disabled = AutomaticFailoverStatus' "disabled"

pattern AutomaticFailoverStatus_Disabling :: AutomaticFailoverStatus
pattern AutomaticFailoverStatus_Disabling = AutomaticFailoverStatus' "disabling"

pattern AutomaticFailoverStatus_Enabled :: AutomaticFailoverStatus
pattern AutomaticFailoverStatus_Enabled = AutomaticFailoverStatus' "enabled"

pattern AutomaticFailoverStatus_Enabling :: AutomaticFailoverStatus
pattern AutomaticFailoverStatus_Enabling = AutomaticFailoverStatus' "enabling"

{-# COMPLETE
  AutomaticFailoverStatus_Disabled,
  AutomaticFailoverStatus_Disabling,
  AutomaticFailoverStatus_Enabled,
  AutomaticFailoverStatus_Enabling,
  AutomaticFailoverStatus'
  #-}

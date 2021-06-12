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
-- Module      : Network.AWS.Greengrass.Types.UpdateTargetsOperatingSystem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.UpdateTargetsOperatingSystem
  ( UpdateTargetsOperatingSystem
      ( ..,
        UpdateTargetsOperatingSystem_Amazon_linux,
        UpdateTargetsOperatingSystem_Openwrt,
        UpdateTargetsOperatingSystem_Raspbian,
        UpdateTargetsOperatingSystem_Ubuntu
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The operating system of the cores which are the targets of an update.
newtype UpdateTargetsOperatingSystem = UpdateTargetsOperatingSystem'
  { fromUpdateTargetsOperatingSystem ::
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

pattern UpdateTargetsOperatingSystem_Amazon_linux :: UpdateTargetsOperatingSystem
pattern UpdateTargetsOperatingSystem_Amazon_linux = UpdateTargetsOperatingSystem' "amazon_linux"

pattern UpdateTargetsOperatingSystem_Openwrt :: UpdateTargetsOperatingSystem
pattern UpdateTargetsOperatingSystem_Openwrt = UpdateTargetsOperatingSystem' "openwrt"

pattern UpdateTargetsOperatingSystem_Raspbian :: UpdateTargetsOperatingSystem
pattern UpdateTargetsOperatingSystem_Raspbian = UpdateTargetsOperatingSystem' "raspbian"

pattern UpdateTargetsOperatingSystem_Ubuntu :: UpdateTargetsOperatingSystem
pattern UpdateTargetsOperatingSystem_Ubuntu = UpdateTargetsOperatingSystem' "ubuntu"

{-# COMPLETE
  UpdateTargetsOperatingSystem_Amazon_linux,
  UpdateTargetsOperatingSystem_Openwrt,
  UpdateTargetsOperatingSystem_Raspbian,
  UpdateTargetsOperatingSystem_Ubuntu,
  UpdateTargetsOperatingSystem'
  #-}

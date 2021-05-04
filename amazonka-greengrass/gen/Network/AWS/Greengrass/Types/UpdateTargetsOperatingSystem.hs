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

import qualified Network.AWS.Prelude as Prelude

-- | The operating system of the cores which are the targets of an update.
newtype UpdateTargetsOperatingSystem = UpdateTargetsOperatingSystem'
  { fromUpdateTargetsOperatingSystem ::
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

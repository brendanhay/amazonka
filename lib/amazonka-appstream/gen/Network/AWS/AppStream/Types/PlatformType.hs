{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.PlatformType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.PlatformType
  ( PlatformType
    ( PlatformType'
    , PlatformTypeWindows
    , PlatformTypeWindowsServer2016
    , PlatformTypeWindowsServer2019
    , fromPlatformType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype PlatformType = PlatformType'{fromPlatformType :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern PlatformTypeWindows :: PlatformType
pattern PlatformTypeWindows = PlatformType' "WINDOWS"

pattern PlatformTypeWindowsServer2016 :: PlatformType
pattern PlatformTypeWindowsServer2016 = PlatformType' "WINDOWS_SERVER_2016"

pattern PlatformTypeWindowsServer2019 :: PlatformType
pattern PlatformTypeWindowsServer2019 = PlatformType' "WINDOWS_SERVER_2019"

{-# COMPLETE 
  PlatformTypeWindows,

  PlatformTypeWindowsServer2016,

  PlatformTypeWindowsServer2019,
  PlatformType'
  #-}

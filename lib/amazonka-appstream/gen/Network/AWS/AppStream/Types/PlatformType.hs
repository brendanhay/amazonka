-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.PlatformType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.PlatformType
  ( PlatformType
      ( PlatformType',
        Windows,
        WindowsServer2016,
        WindowsServer2019
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PlatformType = PlatformType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Windows :: PlatformType
pattern Windows = PlatformType' "WINDOWS"

pattern WindowsServer2016 :: PlatformType
pattern WindowsServer2016 = PlatformType' "WINDOWS_SERVER_2016"

pattern WindowsServer2019 :: PlatformType
pattern WindowsServer2019 = PlatformType' "WINDOWS_SERVER_2019"

{-# COMPLETE
  Windows,
  WindowsServer2016,
  WindowsServer2019,
  PlatformType'
  #-}

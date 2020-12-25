{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.OperatingSystem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.OperatingSystem
  ( OperatingSystem
      ( OperatingSystem',
        OperatingSystemWindows2012,
        OperatingSystemAmazonLinux,
        OperatingSystemAmazonLinux2,
        fromOperatingSystem
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype OperatingSystem = OperatingSystem'
  { fromOperatingSystem ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern OperatingSystemWindows2012 :: OperatingSystem
pattern OperatingSystemWindows2012 = OperatingSystem' "WINDOWS_2012"

pattern OperatingSystemAmazonLinux :: OperatingSystem
pattern OperatingSystemAmazonLinux = OperatingSystem' "AMAZON_LINUX"

pattern OperatingSystemAmazonLinux2 :: OperatingSystem
pattern OperatingSystemAmazonLinux2 = OperatingSystem' "AMAZON_LINUX_2"

{-# COMPLETE
  OperatingSystemWindows2012,
  OperatingSystemAmazonLinux,
  OperatingSystemAmazonLinux2,
  OperatingSystem'
  #-}

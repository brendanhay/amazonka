{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputResolution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputResolution
  ( InputResolution
      ( InputResolution',
        IRSD,
        IRHD,
        IRUhd
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Input resolution based on lines of vertical resolution in the input; SD is less than 720 lines, HD is 720 to 1080 lines, UHD is greater than 1080 lines
newtype InputResolution = InputResolution' Lude.Text
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

pattern IRSD :: InputResolution
pattern IRSD = InputResolution' "SD"

pattern IRHD :: InputResolution
pattern IRHD = InputResolution' "HD"

pattern IRUhd :: InputResolution
pattern IRUhd = InputResolution' "UHD"

{-# COMPLETE
  IRSD,
  IRHD,
  IRUhd,
  InputResolution'
  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ProresCodecProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ProresCodecProfile
  ( ProresCodecProfile
      ( ProresCodecProfile',
        ProresCodecProfileAppleProres422,
        ProresCodecProfileAppleProres422Hq,
        ProresCodecProfileAppleProres422Lt,
        ProresCodecProfileAppleProres422Proxy,
        fromProresCodecProfile
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Use Profile (ProResCodecProfile) to specifiy the type of Apple ProRes codec to use for this output.
newtype ProresCodecProfile = ProresCodecProfile'
  { fromProresCodecProfile ::
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

pattern ProresCodecProfileAppleProres422 :: ProresCodecProfile
pattern ProresCodecProfileAppleProres422 = ProresCodecProfile' "APPLE_PRORES_422"

pattern ProresCodecProfileAppleProres422Hq :: ProresCodecProfile
pattern ProresCodecProfileAppleProres422Hq = ProresCodecProfile' "APPLE_PRORES_422_HQ"

pattern ProresCodecProfileAppleProres422Lt :: ProresCodecProfile
pattern ProresCodecProfileAppleProres422Lt = ProresCodecProfile' "APPLE_PRORES_422_LT"

pattern ProresCodecProfileAppleProres422Proxy :: ProresCodecProfile
pattern ProresCodecProfileAppleProres422Proxy = ProresCodecProfile' "APPLE_PRORES_422_PROXY"

{-# COMPLETE
  ProresCodecProfileAppleProres422,
  ProresCodecProfileAppleProres422Hq,
  ProresCodecProfileAppleProres422Lt,
  ProresCodecProfileAppleProres422Proxy,
  ProresCodecProfile'
  #-}

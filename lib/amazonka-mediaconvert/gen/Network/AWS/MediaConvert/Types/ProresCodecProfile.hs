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
        AppleProres422,
        AppleProres422Hq,
        AppleProres422LT,
        AppleProres422Proxy
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Use Profile (ProResCodecProfile) to specifiy the type of Apple ProRes codec to use for this output.
newtype ProresCodecProfile = ProresCodecProfile' Lude.Text
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

pattern AppleProres422 :: ProresCodecProfile
pattern AppleProres422 = ProresCodecProfile' "APPLE_PRORES_422"

pattern AppleProres422Hq :: ProresCodecProfile
pattern AppleProres422Hq = ProresCodecProfile' "APPLE_PRORES_422_HQ"

pattern AppleProres422LT :: ProresCodecProfile
pattern AppleProres422LT = ProresCodecProfile' "APPLE_PRORES_422_LT"

pattern AppleProres422Proxy :: ProresCodecProfile
pattern AppleProres422Proxy = ProresCodecProfile' "APPLE_PRORES_422_PROXY"

{-# COMPLETE
  AppleProres422,
  AppleProres422Hq,
  AppleProres422LT,
  AppleProres422Proxy,
  ProresCodecProfile'
  #-}

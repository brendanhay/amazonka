{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AacProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AacProfile
  ( AacProfile
      ( AacProfile',
        APHEV1,
        APHEV2,
        APLC
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Aac Profile
newtype AacProfile = AacProfile' Lude.Text
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

pattern APHEV1 :: AacProfile
pattern APHEV1 = AacProfile' "HEV1"

pattern APHEV2 :: AacProfile
pattern APHEV2 = AacProfile' "HEV2"

pattern APLC :: AacProfile
pattern APLC = AacProfile' "LC"

{-# COMPLETE
  APHEV1,
  APHEV2,
  APLC,
  AacProfile'
  #-}

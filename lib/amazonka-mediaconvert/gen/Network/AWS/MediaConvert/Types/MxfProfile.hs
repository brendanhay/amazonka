{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MxfProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MxfProfile
  ( MxfProfile
      ( MxfProfile',
        MxfProfileD10,
        MxfProfileXdcam,
        MxfProfileOP1A,
        fromMxfProfile
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Specify the MXF profile, also called shim, for this output. When you choose Auto, MediaConvert chooses a profile based on the video codec and resolution. For a list of codecs supported with each MXF profile, see https://docs.aws.amazon.com/mediaconvert/latest/ug/codecs-supported-with-each-mxf-profile.html. For more information about the automatic selection behavior, see https://docs.aws.amazon.com/mediaconvert/latest/ug/default-automatic-selection-of-mxf-profiles.html.
newtype MxfProfile = MxfProfile' {fromMxfProfile :: Core.Text}
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

pattern MxfProfileD10 :: MxfProfile
pattern MxfProfileD10 = MxfProfile' "D_10"

pattern MxfProfileXdcam :: MxfProfile
pattern MxfProfileXdcam = MxfProfile' "XDCAM"

pattern MxfProfileOP1A :: MxfProfile
pattern MxfProfileOP1A = MxfProfile' "OP1A"

{-# COMPLETE
  MxfProfileD10,
  MxfProfileXdcam,
  MxfProfileOP1A,
  MxfProfile'
  #-}

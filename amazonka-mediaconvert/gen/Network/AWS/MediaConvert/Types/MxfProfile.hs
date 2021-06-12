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
-- Module      : Network.AWS.MediaConvert.Types.MxfProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MxfProfile
  ( MxfProfile
      ( ..,
        MxfProfile_D_10,
        MxfProfile_OP1A,
        MxfProfile_XDCAM
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Specify the MXF profile, also called shim, for this output. When you
-- choose Auto, MediaConvert chooses a profile based on the video codec and
-- resolution. For a list of codecs supported with each MXF profile, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/codecs-supported-with-each-mxf-profile.html.
-- For more information about the automatic selection behavior, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/default-automatic-selection-of-mxf-profiles.html.
newtype MxfProfile = MxfProfile'
  { fromMxfProfile ::
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

pattern MxfProfile_D_10 :: MxfProfile
pattern MxfProfile_D_10 = MxfProfile' "D_10"

pattern MxfProfile_OP1A :: MxfProfile
pattern MxfProfile_OP1A = MxfProfile' "OP1A"

pattern MxfProfile_XDCAM :: MxfProfile
pattern MxfProfile_XDCAM = MxfProfile' "XDCAM"

{-# COMPLETE
  MxfProfile_D_10,
  MxfProfile_OP1A,
  MxfProfile_XDCAM,
  MxfProfile'
  #-}

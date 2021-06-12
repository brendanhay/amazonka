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
-- Module      : Network.AWS.MediaConvert.Types.Eac3StereoDownmix
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3StereoDownmix
  ( Eac3StereoDownmix
      ( ..,
        Eac3StereoDownmix_DPL2,
        Eac3StereoDownmix_LO_RO,
        Eac3StereoDownmix_LT_RT,
        Eac3StereoDownmix_NOT_INDICATED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Choose how the service does stereo downmixing. This setting only applies
-- if you keep the default value of 3\/2 - L, R, C, Ls, Rs
-- (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you
-- choose a different value for Coding mode, the service ignores Stereo
-- downmix (Eac3StereoDownmix).
newtype Eac3StereoDownmix = Eac3StereoDownmix'
  { fromEac3StereoDownmix ::
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

pattern Eac3StereoDownmix_DPL2 :: Eac3StereoDownmix
pattern Eac3StereoDownmix_DPL2 = Eac3StereoDownmix' "DPL2"

pattern Eac3StereoDownmix_LO_RO :: Eac3StereoDownmix
pattern Eac3StereoDownmix_LO_RO = Eac3StereoDownmix' "LO_RO"

pattern Eac3StereoDownmix_LT_RT :: Eac3StereoDownmix
pattern Eac3StereoDownmix_LT_RT = Eac3StereoDownmix' "LT_RT"

pattern Eac3StereoDownmix_NOT_INDICATED :: Eac3StereoDownmix
pattern Eac3StereoDownmix_NOT_INDICATED = Eac3StereoDownmix' "NOT_INDICATED"

{-# COMPLETE
  Eac3StereoDownmix_DPL2,
  Eac3StereoDownmix_LO_RO,
  Eac3StereoDownmix_LT_RT,
  Eac3StereoDownmix_NOT_INDICATED,
  Eac3StereoDownmix'
  #-}

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
-- Module      : Network.AWS.MediaConvert.Types.Ac3DynamicRangeCompressionProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Ac3DynamicRangeCompressionProfile
  ( Ac3DynamicRangeCompressionProfile
      ( ..,
        Ac3DynamicRangeCompressionProfile_FILM_STANDARD,
        Ac3DynamicRangeCompressionProfile_NONE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | If set to FILM_STANDARD, adds dynamic range compression signaling to the
-- output bitstream as defined in the Dolby Digital specification.
newtype Ac3DynamicRangeCompressionProfile = Ac3DynamicRangeCompressionProfile'
  { fromAc3DynamicRangeCompressionProfile ::
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

pattern Ac3DynamicRangeCompressionProfile_FILM_STANDARD :: Ac3DynamicRangeCompressionProfile
pattern Ac3DynamicRangeCompressionProfile_FILM_STANDARD = Ac3DynamicRangeCompressionProfile' "FILM_STANDARD"

pattern Ac3DynamicRangeCompressionProfile_NONE :: Ac3DynamicRangeCompressionProfile
pattern Ac3DynamicRangeCompressionProfile_NONE = Ac3DynamicRangeCompressionProfile' "NONE"

{-# COMPLETE
  Ac3DynamicRangeCompressionProfile_FILM_STANDARD,
  Ac3DynamicRangeCompressionProfile_NONE,
  Ac3DynamicRangeCompressionProfile'
  #-}

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
import qualified Network.AWS.Prelude as Prelude

-- | If set to FILM_STANDARD, adds dynamic range compression signaling to the
-- output bitstream as defined in the Dolby Digital specification.
newtype Ac3DynamicRangeCompressionProfile = Ac3DynamicRangeCompressionProfile'
  { fromAc3DynamicRangeCompressionProfile ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
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

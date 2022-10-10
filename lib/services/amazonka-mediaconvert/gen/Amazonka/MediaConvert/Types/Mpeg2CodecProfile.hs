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
-- Module      : Amazonka.MediaConvert.Types.Mpeg2CodecProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Mpeg2CodecProfile
  ( Mpeg2CodecProfile
      ( ..,
        Mpeg2CodecProfile_MAIN,
        Mpeg2CodecProfile_PROFILE_422
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Use Profile (Mpeg2CodecProfile) to set the MPEG-2 profile for the video
-- output.
newtype Mpeg2CodecProfile = Mpeg2CodecProfile'
  { fromMpeg2CodecProfile ::
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

pattern Mpeg2CodecProfile_MAIN :: Mpeg2CodecProfile
pattern Mpeg2CodecProfile_MAIN = Mpeg2CodecProfile' "MAIN"

pattern Mpeg2CodecProfile_PROFILE_422 :: Mpeg2CodecProfile
pattern Mpeg2CodecProfile_PROFILE_422 = Mpeg2CodecProfile' "PROFILE_422"

{-# COMPLETE
  Mpeg2CodecProfile_MAIN,
  Mpeg2CodecProfile_PROFILE_422,
  Mpeg2CodecProfile'
  #-}

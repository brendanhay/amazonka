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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use Profile (Mpeg2CodecProfile) to set the MPEG-2 profile for the video
-- output.
newtype Mpeg2CodecProfile = Mpeg2CodecProfile'
  { fromMpeg2CodecProfile ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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

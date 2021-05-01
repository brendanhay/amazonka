{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2CodecProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2CodecProfile
  ( Mpeg2CodecProfile
      ( ..,
        Mpeg2CodecProfile_MAIN,
        Mpeg2CodecProfile_PROFILE_422
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Use Profile (Mpeg2CodecProfile) to set the MPEG-2 profile for the video
-- output.
newtype Mpeg2CodecProfile = Mpeg2CodecProfile'
  { fromMpeg2CodecProfile ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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

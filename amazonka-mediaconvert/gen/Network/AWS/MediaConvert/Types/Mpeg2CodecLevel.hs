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
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2CodecLevel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2CodecLevel
  ( Mpeg2CodecLevel
      ( ..,
        Mpeg2CodecLevel_AUTO,
        Mpeg2CodecLevel_HIGH,
        Mpeg2CodecLevel_HIGH1440,
        Mpeg2CodecLevel_LOW,
        Mpeg2CodecLevel_MAIN
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Use Level (Mpeg2CodecLevel) to set the MPEG-2 level for the video
-- output.
newtype Mpeg2CodecLevel = Mpeg2CodecLevel'
  { fromMpeg2CodecLevel ::
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

pattern Mpeg2CodecLevel_AUTO :: Mpeg2CodecLevel
pattern Mpeg2CodecLevel_AUTO = Mpeg2CodecLevel' "AUTO"

pattern Mpeg2CodecLevel_HIGH :: Mpeg2CodecLevel
pattern Mpeg2CodecLevel_HIGH = Mpeg2CodecLevel' "HIGH"

pattern Mpeg2CodecLevel_HIGH1440 :: Mpeg2CodecLevel
pattern Mpeg2CodecLevel_HIGH1440 = Mpeg2CodecLevel' "HIGH1440"

pattern Mpeg2CodecLevel_LOW :: Mpeg2CodecLevel
pattern Mpeg2CodecLevel_LOW = Mpeg2CodecLevel' "LOW"

pattern Mpeg2CodecLevel_MAIN :: Mpeg2CodecLevel
pattern Mpeg2CodecLevel_MAIN = Mpeg2CodecLevel' "MAIN"

{-# COMPLETE
  Mpeg2CodecLevel_AUTO,
  Mpeg2CodecLevel_HIGH,
  Mpeg2CodecLevel_HIGH1440,
  Mpeg2CodecLevel_LOW,
  Mpeg2CodecLevel_MAIN,
  Mpeg2CodecLevel'
  #-}

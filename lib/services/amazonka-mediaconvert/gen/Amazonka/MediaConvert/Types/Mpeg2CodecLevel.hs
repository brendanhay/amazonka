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
-- Module      : Amazonka.MediaConvert.Types.Mpeg2CodecLevel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Mpeg2CodecLevel
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use Level (Mpeg2CodecLevel) to set the MPEG-2 level for the video
-- output.
newtype Mpeg2CodecLevel = Mpeg2CodecLevel'
  { fromMpeg2CodecLevel ::
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

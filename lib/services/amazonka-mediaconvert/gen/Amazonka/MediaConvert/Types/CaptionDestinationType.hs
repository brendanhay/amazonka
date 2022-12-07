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
-- Module      : Amazonka.MediaConvert.Types.CaptionDestinationType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.CaptionDestinationType
  ( CaptionDestinationType
      ( ..,
        CaptionDestinationType_BURN_IN,
        CaptionDestinationType_DVB_SUB,
        CaptionDestinationType_EMBEDDED,
        CaptionDestinationType_EMBEDDED_PLUS_SCTE20,
        CaptionDestinationType_IMSC,
        CaptionDestinationType_SCC,
        CaptionDestinationType_SCTE20_PLUS_EMBEDDED,
        CaptionDestinationType_SMI,
        CaptionDestinationType_SRT,
        CaptionDestinationType_TELETEXT,
        CaptionDestinationType_TTML,
        CaptionDestinationType_WEBVTT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the format for this set of captions on this output. The default
-- format is embedded without SCTE-20. Note that your choice of video
-- output container constrains your choice of output captions format. For
-- more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/captions-support-tables.html.
-- If you are using SCTE-20 and you want to create an output that complies
-- with the SCTE-43 spec, choose SCTE-20 plus embedded
-- (SCTE20_PLUS_EMBEDDED). To create a non-compliant output where the
-- embedded captions come first, choose Embedded plus SCTE-20
-- (EMBEDDED_PLUS_SCTE20).
newtype CaptionDestinationType = CaptionDestinationType'
  { fromCaptionDestinationType ::
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

pattern CaptionDestinationType_BURN_IN :: CaptionDestinationType
pattern CaptionDestinationType_BURN_IN = CaptionDestinationType' "BURN_IN"

pattern CaptionDestinationType_DVB_SUB :: CaptionDestinationType
pattern CaptionDestinationType_DVB_SUB = CaptionDestinationType' "DVB_SUB"

pattern CaptionDestinationType_EMBEDDED :: CaptionDestinationType
pattern CaptionDestinationType_EMBEDDED = CaptionDestinationType' "EMBEDDED"

pattern CaptionDestinationType_EMBEDDED_PLUS_SCTE20 :: CaptionDestinationType
pattern CaptionDestinationType_EMBEDDED_PLUS_SCTE20 = CaptionDestinationType' "EMBEDDED_PLUS_SCTE20"

pattern CaptionDestinationType_IMSC :: CaptionDestinationType
pattern CaptionDestinationType_IMSC = CaptionDestinationType' "IMSC"

pattern CaptionDestinationType_SCC :: CaptionDestinationType
pattern CaptionDestinationType_SCC = CaptionDestinationType' "SCC"

pattern CaptionDestinationType_SCTE20_PLUS_EMBEDDED :: CaptionDestinationType
pattern CaptionDestinationType_SCTE20_PLUS_EMBEDDED = CaptionDestinationType' "SCTE20_PLUS_EMBEDDED"

pattern CaptionDestinationType_SMI :: CaptionDestinationType
pattern CaptionDestinationType_SMI = CaptionDestinationType' "SMI"

pattern CaptionDestinationType_SRT :: CaptionDestinationType
pattern CaptionDestinationType_SRT = CaptionDestinationType' "SRT"

pattern CaptionDestinationType_TELETEXT :: CaptionDestinationType
pattern CaptionDestinationType_TELETEXT = CaptionDestinationType' "TELETEXT"

pattern CaptionDestinationType_TTML :: CaptionDestinationType
pattern CaptionDestinationType_TTML = CaptionDestinationType' "TTML"

pattern CaptionDestinationType_WEBVTT :: CaptionDestinationType
pattern CaptionDestinationType_WEBVTT = CaptionDestinationType' "WEBVTT"

{-# COMPLETE
  CaptionDestinationType_BURN_IN,
  CaptionDestinationType_DVB_SUB,
  CaptionDestinationType_EMBEDDED,
  CaptionDestinationType_EMBEDDED_PLUS_SCTE20,
  CaptionDestinationType_IMSC,
  CaptionDestinationType_SCC,
  CaptionDestinationType_SCTE20_PLUS_EMBEDDED,
  CaptionDestinationType_SMI,
  CaptionDestinationType_SRT,
  CaptionDestinationType_TELETEXT,
  CaptionDestinationType_TTML,
  CaptionDestinationType_WEBVTT,
  CaptionDestinationType'
  #-}

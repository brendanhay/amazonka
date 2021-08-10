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
-- Module      : Network.AWS.MediaConvert.Types.CaptionSourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CaptionSourceType
  ( CaptionSourceType
      ( ..,
        CaptionSourceType_ANCILLARY,
        CaptionSourceType_DVB_SUB,
        CaptionSourceType_EMBEDDED,
        CaptionSourceType_IMSC,
        CaptionSourceType_NULL_SOURCE,
        CaptionSourceType_SCC,
        CaptionSourceType_SCTE20,
        CaptionSourceType_SMI,
        CaptionSourceType_SMPTE_TT,
        CaptionSourceType_SRT,
        CaptionSourceType_STL,
        CaptionSourceType_TELETEXT,
        CaptionSourceType_TTML
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Use Source (SourceType) to identify the format of your input captions.
-- The service cannot auto-detect caption format.
newtype CaptionSourceType = CaptionSourceType'
  { fromCaptionSourceType ::
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

pattern CaptionSourceType_ANCILLARY :: CaptionSourceType
pattern CaptionSourceType_ANCILLARY = CaptionSourceType' "ANCILLARY"

pattern CaptionSourceType_DVB_SUB :: CaptionSourceType
pattern CaptionSourceType_DVB_SUB = CaptionSourceType' "DVB_SUB"

pattern CaptionSourceType_EMBEDDED :: CaptionSourceType
pattern CaptionSourceType_EMBEDDED = CaptionSourceType' "EMBEDDED"

pattern CaptionSourceType_IMSC :: CaptionSourceType
pattern CaptionSourceType_IMSC = CaptionSourceType' "IMSC"

pattern CaptionSourceType_NULL_SOURCE :: CaptionSourceType
pattern CaptionSourceType_NULL_SOURCE = CaptionSourceType' "NULL_SOURCE"

pattern CaptionSourceType_SCC :: CaptionSourceType
pattern CaptionSourceType_SCC = CaptionSourceType' "SCC"

pattern CaptionSourceType_SCTE20 :: CaptionSourceType
pattern CaptionSourceType_SCTE20 = CaptionSourceType' "SCTE20"

pattern CaptionSourceType_SMI :: CaptionSourceType
pattern CaptionSourceType_SMI = CaptionSourceType' "SMI"

pattern CaptionSourceType_SMPTE_TT :: CaptionSourceType
pattern CaptionSourceType_SMPTE_TT = CaptionSourceType' "SMPTE_TT"

pattern CaptionSourceType_SRT :: CaptionSourceType
pattern CaptionSourceType_SRT = CaptionSourceType' "SRT"

pattern CaptionSourceType_STL :: CaptionSourceType
pattern CaptionSourceType_STL = CaptionSourceType' "STL"

pattern CaptionSourceType_TELETEXT :: CaptionSourceType
pattern CaptionSourceType_TELETEXT = CaptionSourceType' "TELETEXT"

pattern CaptionSourceType_TTML :: CaptionSourceType
pattern CaptionSourceType_TTML = CaptionSourceType' "TTML"

{-# COMPLETE
  CaptionSourceType_ANCILLARY,
  CaptionSourceType_DVB_SUB,
  CaptionSourceType_EMBEDDED,
  CaptionSourceType_IMSC,
  CaptionSourceType_NULL_SOURCE,
  CaptionSourceType_SCC,
  CaptionSourceType_SCTE20,
  CaptionSourceType_SMI,
  CaptionSourceType_SMPTE_TT,
  CaptionSourceType_SRT,
  CaptionSourceType_STL,
  CaptionSourceType_TELETEXT,
  CaptionSourceType_TTML,
  CaptionSourceType'
  #-}

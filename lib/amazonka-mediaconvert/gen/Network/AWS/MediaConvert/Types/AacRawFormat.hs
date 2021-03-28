{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AacRawFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AacRawFormat
  ( AacRawFormat
    ( AacRawFormat'
    , AacRawFormatLatmLoas
    , AacRawFormatNone
    , fromAacRawFormat
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Enables LATM/LOAS AAC output. Note that if you use LATM/LOAS AAC in an output, you must choose "No container" for the output container.
newtype AacRawFormat = AacRawFormat'{fromAacRawFormat :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern AacRawFormatLatmLoas :: AacRawFormat
pattern AacRawFormatLatmLoas = AacRawFormat' "LATM_LOAS"

pattern AacRawFormatNone :: AacRawFormat
pattern AacRawFormatNone = AacRawFormat' "NONE"

{-# COMPLETE 
  AacRawFormatLatmLoas,

  AacRawFormatNone,
  AacRawFormat'
  #-}

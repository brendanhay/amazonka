{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NielsenActiveWatermarkProcessType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NielsenActiveWatermarkProcessType
  ( NielsenActiveWatermarkProcessType
      ( NielsenActiveWatermarkProcessType',
        NielsenActiveWatermarkProcessTypeNAES2AndNw,
        NielsenActiveWatermarkProcessTypeCbet,
        NielsenActiveWatermarkProcessTypeNAES2AndNwAndCbet,
        fromNielsenActiveWatermarkProcessType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Choose the type of Nielsen watermarks that you want in your outputs. When you choose NAES 2 and NW (NAES2_AND_NW), you must provide a value for the setting SID (sourceId). When you choose CBET (CBET), you must provide a value for the setting CSID (cbetSourceId). When you choose NAES 2, NW, and CBET (NAES2_AND_NW_AND_CBET), you must provide values for both of these settings.
newtype NielsenActiveWatermarkProcessType = NielsenActiveWatermarkProcessType'
  { fromNielsenActiveWatermarkProcessType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern NielsenActiveWatermarkProcessTypeNAES2AndNw :: NielsenActiveWatermarkProcessType
pattern NielsenActiveWatermarkProcessTypeNAES2AndNw = NielsenActiveWatermarkProcessType' "NAES2_AND_NW"

pattern NielsenActiveWatermarkProcessTypeCbet :: NielsenActiveWatermarkProcessType
pattern NielsenActiveWatermarkProcessTypeCbet = NielsenActiveWatermarkProcessType' "CBET"

pattern NielsenActiveWatermarkProcessTypeNAES2AndNwAndCbet :: NielsenActiveWatermarkProcessType
pattern NielsenActiveWatermarkProcessTypeNAES2AndNwAndCbet = NielsenActiveWatermarkProcessType' "NAES2_AND_NW_AND_CBET"

{-# COMPLETE
  NielsenActiveWatermarkProcessTypeNAES2AndNw,
  NielsenActiveWatermarkProcessTypeCbet,
  NielsenActiveWatermarkProcessTypeNAES2AndNwAndCbet,
  NielsenActiveWatermarkProcessType'
  #-}

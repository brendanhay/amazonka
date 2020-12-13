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
        NAES2AndNw,
        Cbet,
        NAES2AndNwAndCbet
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Choose the type of Nielsen watermarks that you want in your outputs. When you choose NAES 2 and NW (NAES2_AND_NW), you must provide a value for the setting SID (sourceId). When you choose CBET (CBET), you must provide a value for the setting CSID (cbetSourceId). When you choose NAES 2, NW, and CBET (NAES2_AND_NW_AND_CBET), you must provide values for both of these settings.
newtype NielsenActiveWatermarkProcessType = NielsenActiveWatermarkProcessType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern NAES2AndNw :: NielsenActiveWatermarkProcessType
pattern NAES2AndNw = NielsenActiveWatermarkProcessType' "NAES2_AND_NW"

pattern Cbet :: NielsenActiveWatermarkProcessType
pattern Cbet = NielsenActiveWatermarkProcessType' "CBET"

pattern NAES2AndNwAndCbet :: NielsenActiveWatermarkProcessType
pattern NAES2AndNwAndCbet = NielsenActiveWatermarkProcessType' "NAES2_AND_NW_AND_CBET"

{-# COMPLETE
  NAES2AndNw,
  Cbet,
  NAES2AndNwAndCbet,
  NielsenActiveWatermarkProcessType'
  #-}

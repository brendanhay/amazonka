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
-- Module      : Network.AWS.MediaConvert.Types.NielsenActiveWatermarkProcessType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NielsenActiveWatermarkProcessType
  ( NielsenActiveWatermarkProcessType
      ( ..,
        NielsenActiveWatermarkProcessType_CBET,
        NielsenActiveWatermarkProcessType_NAES2_AND_NW,
        NielsenActiveWatermarkProcessType_NAES2_AND_NW_AND_CBET
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Choose the type of Nielsen watermarks that you want in your outputs.
-- When you choose NAES 2 and NW (NAES2_AND_NW), you must provide a value
-- for the setting SID (sourceId). When you choose CBET (CBET), you must
-- provide a value for the setting CSID (cbetSourceId). When you choose
-- NAES 2, NW, and CBET (NAES2_AND_NW_AND_CBET), you must provide values
-- for both of these settings.
newtype NielsenActiveWatermarkProcessType = NielsenActiveWatermarkProcessType'
  { fromNielsenActiveWatermarkProcessType ::
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

pattern NielsenActiveWatermarkProcessType_CBET :: NielsenActiveWatermarkProcessType
pattern NielsenActiveWatermarkProcessType_CBET = NielsenActiveWatermarkProcessType' "CBET"

pattern NielsenActiveWatermarkProcessType_NAES2_AND_NW :: NielsenActiveWatermarkProcessType
pattern NielsenActiveWatermarkProcessType_NAES2_AND_NW = NielsenActiveWatermarkProcessType' "NAES2_AND_NW"

pattern NielsenActiveWatermarkProcessType_NAES2_AND_NW_AND_CBET :: NielsenActiveWatermarkProcessType
pattern NielsenActiveWatermarkProcessType_NAES2_AND_NW_AND_CBET = NielsenActiveWatermarkProcessType' "NAES2_AND_NW_AND_CBET"

{-# COMPLETE
  NielsenActiveWatermarkProcessType_CBET,
  NielsenActiveWatermarkProcessType_NAES2_AND_NW,
  NielsenActiveWatermarkProcessType_NAES2_AND_NW_AND_CBET,
  NielsenActiveWatermarkProcessType'
  #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NielsenActiveWatermarkProcessType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NielsenActiveWatermarkProcessType where

import Network.AWS.Prelude

-- | Choose the type of Nielsen watermarks that you want in your outputs. When you choose NAES 2 and NW (NAES2_AND_NW), you must provide a value for the setting SID (sourceId). When you choose CBET (CBET), you must provide a value for the setting CSID (cbetSourceId). When you choose NAES 2, NW, and CBET (NAES2_AND_NW_AND_CBET), you must provide values for both of these settings.
data NielsenActiveWatermarkProcessType
  = Cbet
  | NAES2AndNw
  | NAES2AndNwAndCbet
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText NielsenActiveWatermarkProcessType where
  parser =
    takeLowerText >>= \case
      "cbet" -> pure Cbet
      "naes2_and_nw" -> pure NAES2AndNw
      "naes2_and_nw_and_cbet" -> pure NAES2AndNwAndCbet
      e ->
        fromTextError $
          "Failure parsing NielsenActiveWatermarkProcessType from value: '" <> e
            <> "'. Accepted values: cbet, naes2_and_nw, naes2_and_nw_and_cbet"

instance ToText NielsenActiveWatermarkProcessType where
  toText = \case
    Cbet -> "CBET"
    NAES2AndNw -> "NAES2_AND_NW"
    NAES2AndNwAndCbet -> "NAES2_AND_NW_AND_CBET"

instance Hashable NielsenActiveWatermarkProcessType

instance NFData NielsenActiveWatermarkProcessType

instance ToByteString NielsenActiveWatermarkProcessType

instance ToQuery NielsenActiveWatermarkProcessType

instance ToHeader NielsenActiveWatermarkProcessType

instance ToJSON NielsenActiveWatermarkProcessType where
  toJSON = toJSONText

instance FromJSON NielsenActiveWatermarkProcessType where
  parseJSON = parseJSONText "NielsenActiveWatermarkProcessType"

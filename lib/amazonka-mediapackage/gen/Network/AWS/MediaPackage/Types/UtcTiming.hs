{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.UtcTiming
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.UtcTiming where

import Network.AWS.Prelude

data UtcTiming
  = UTHTTPHead
  | UTHTTPIso
  | UTNone
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

instance FromText UtcTiming where
  parser =
    takeLowerText >>= \case
      "http-head" -> pure UTHTTPHead
      "http-iso" -> pure UTHTTPIso
      "none" -> pure UTNone
      e ->
        fromTextError $
          "Failure parsing UtcTiming from value: '" <> e
            <> "'. Accepted values: http-head, http-iso, none"

instance ToText UtcTiming where
  toText = \case
    UTHTTPHead -> "HTTP-HEAD"
    UTHTTPIso -> "HTTP-ISO"
    UTNone -> "NONE"

instance Hashable UtcTiming

instance NFData UtcTiming

instance ToByteString UtcTiming

instance ToQuery UtcTiming

instance ToHeader UtcTiming

instance ToJSON UtcTiming where
  toJSON = toJSONText

instance FromJSON UtcTiming where
  parseJSON = parseJSONText "UtcTiming"

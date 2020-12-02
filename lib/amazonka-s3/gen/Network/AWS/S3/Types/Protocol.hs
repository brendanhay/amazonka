{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Protocol
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Protocol where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data Protocol
  = HTTP
  | HTTPS
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

instance FromText Protocol where
  parser =
    takeLowerText >>= \case
      "http" -> pure HTTP
      "https" -> pure HTTPS
      e ->
        fromTextError $
          "Failure parsing Protocol from value: '" <> e
            <> "'. Accepted values: http, https"

instance ToText Protocol where
  toText = \case
    HTTP -> "http"
    HTTPS -> "https"

instance Hashable Protocol

instance NFData Protocol

instance ToByteString Protocol

instance ToQuery Protocol

instance ToHeader Protocol

instance FromXML Protocol where
  parseXML = parseXMLText "Protocol"

instance ToXML Protocol where
  toXML = toXMLText

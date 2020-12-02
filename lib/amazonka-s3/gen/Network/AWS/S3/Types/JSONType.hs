{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.JSONType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.JSONType where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data JSONType
  = Document
  | Lines
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

instance FromText JSONType where
  parser =
    takeLowerText >>= \case
      "document" -> pure Document
      "lines" -> pure Lines
      e ->
        fromTextError $
          "Failure parsing JSONType from value: '" <> e
            <> "'. Accepted values: document, lines"

instance ToText JSONType where
  toText = \case
    Document -> "DOCUMENT"
    Lines -> "LINES"

instance Hashable JSONType

instance NFData JSONType

instance ToByteString JSONType

instance ToQuery JSONType

instance ToHeader JSONType

instance ToXML JSONType where
  toXML = toXMLText

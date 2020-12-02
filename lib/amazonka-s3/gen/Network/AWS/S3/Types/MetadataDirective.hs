{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.MetadataDirective
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.MetadataDirective where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data MetadataDirective
  = MDCopy
  | MDReplace
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

instance FromText MetadataDirective where
  parser =
    takeLowerText >>= \case
      "copy" -> pure MDCopy
      "replace" -> pure MDReplace
      e ->
        fromTextError $
          "Failure parsing MetadataDirective from value: '" <> e
            <> "'. Accepted values: copy, replace"

instance ToText MetadataDirective where
  toText = \case
    MDCopy -> "COPY"
    MDReplace -> "REPLACE"

instance Hashable MetadataDirective

instance NFData MetadataDirective

instance ToByteString MetadataDirective

instance ToQuery MetadataDirective

instance ToHeader MetadataDirective

instance ToXML MetadataDirective where
  toXML = toXMLText

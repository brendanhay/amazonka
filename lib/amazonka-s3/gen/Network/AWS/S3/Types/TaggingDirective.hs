{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.TaggingDirective
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.TaggingDirective where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data TaggingDirective
  = Copy
  | Replace
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

instance FromText TaggingDirective where
  parser =
    takeLowerText >>= \case
      "copy" -> pure Copy
      "replace" -> pure Replace
      e ->
        fromTextError $
          "Failure parsing TaggingDirective from value: '" <> e
            <> "'. Accepted values: copy, replace"

instance ToText TaggingDirective where
  toText = \case
    Copy -> "COPY"
    Replace -> "REPLACE"

instance Hashable TaggingDirective

instance NFData TaggingDirective

instance ToByteString TaggingDirective

instance ToQuery TaggingDirective

instance ToHeader TaggingDirective

instance ToXML TaggingDirective where
  toXML = toXMLText

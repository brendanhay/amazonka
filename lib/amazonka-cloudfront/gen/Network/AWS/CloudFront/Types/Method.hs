{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Method
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Method where

import Network.AWS.Prelude

data Method
  = Delete
  | Get
  | Head
  | Options
  | Patch
  | Post
  | Put
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

instance FromText Method where
  parser =
    takeLowerText >>= \case
      "delete" -> pure Delete
      "get" -> pure Get
      "head" -> pure Head
      "options" -> pure Options
      "patch" -> pure Patch
      "post" -> pure Post
      "put" -> pure Put
      e ->
        fromTextError $
          "Failure parsing Method from value: '" <> e
            <> "'. Accepted values: delete, get, head, options, patch, post, put"

instance ToText Method where
  toText = \case
    Delete -> "DELETE"
    Get -> "GET"
    Head -> "HEAD"
    Options -> "OPTIONS"
    Patch -> "PATCH"
    Post -> "POST"
    Put -> "PUT"

instance Hashable Method

instance NFData Method

instance ToByteString Method

instance ToQuery Method

instance ToHeader Method

instance FromXML Method where
  parseXML = parseXMLText "Method"

instance ToXML Method where
  toXML = toXMLText

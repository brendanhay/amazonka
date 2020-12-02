{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Types.MethodName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStore.Types.MethodName where

import Network.AWS.Prelude

data MethodName
  = Delete
  | Get
  | Head
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

instance FromText MethodName where
  parser =
    takeLowerText >>= \case
      "delete" -> pure Delete
      "get" -> pure Get
      "head" -> pure Head
      "put" -> pure Put
      e ->
        fromTextError $
          "Failure parsing MethodName from value: '" <> e
            <> "'. Accepted values: delete, get, head, put"

instance ToText MethodName where
  toText = \case
    Delete -> "DELETE"
    Get -> "GET"
    Head -> "HEAD"
    Put -> "PUT"

instance Hashable MethodName

instance NFData MethodName

instance ToByteString MethodName

instance ToQuery MethodName

instance ToHeader MethodName

instance ToJSON MethodName where
  toJSON = toJSONText

instance FromJSON MethodName where
  parseJSON = parseJSONText "MethodName"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentParameterType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentParameterType where

import Network.AWS.Prelude

data DocumentParameterType
  = DPTString
  | DPTStringList
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

instance FromText DocumentParameterType where
  parser =
    takeLowerText >>= \case
      "string" -> pure DPTString
      "stringlist" -> pure DPTStringList
      e ->
        fromTextError $
          "Failure parsing DocumentParameterType from value: '" <> e
            <> "'. Accepted values: string, stringlist"

instance ToText DocumentParameterType where
  toText = \case
    DPTString -> "String"
    DPTStringList -> "StringList"

instance Hashable DocumentParameterType

instance NFData DocumentParameterType

instance ToByteString DocumentParameterType

instance ToQuery DocumentParameterType

instance ToHeader DocumentParameterType

instance FromJSON DocumentParameterType where
  parseJSON = parseJSONText "DocumentParameterType"

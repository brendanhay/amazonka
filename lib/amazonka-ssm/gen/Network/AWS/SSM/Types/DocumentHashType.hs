{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentHashType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentHashType where

import Network.AWS.Prelude

data DocumentHashType
  = HashSHA1
  | HashSHA256
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

instance FromText DocumentHashType where
  parser =
    takeLowerText >>= \case
      "sha1" -> pure HashSHA1
      "sha256" -> pure HashSHA256
      e ->
        fromTextError $
          "Failure parsing DocumentHashType from value: '" <> e
            <> "'. Accepted values: sha1, sha256"

instance ToText DocumentHashType where
  toText = \case
    HashSHA1 -> "Sha1"
    HashSHA256 -> "Sha256"

instance Hashable DocumentHashType

instance NFData DocumentHashType

instance ToByteString DocumentHashType

instance ToQuery DocumentHashType

instance ToHeader DocumentHashType

instance ToJSON DocumentHashType where
  toJSON = toJSONText

instance FromJSON DocumentHashType where
  parseJSON = parseJSONText "DocumentHashType"

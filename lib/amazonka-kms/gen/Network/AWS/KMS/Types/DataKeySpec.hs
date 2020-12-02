{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.DataKeySpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.DataKeySpec where

import Network.AWS.Prelude

data DataKeySpec
  = AES128
  | AES256
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

instance FromText DataKeySpec where
  parser =
    takeLowerText >>= \case
      "aes_128" -> pure AES128
      "aes_256" -> pure AES256
      e ->
        fromTextError $
          "Failure parsing DataKeySpec from value: '" <> e
            <> "'. Accepted values: aes_128, aes_256"

instance ToText DataKeySpec where
  toText = \case
    AES128 -> "AES_128"
    AES256 -> "AES_256"

instance Hashable DataKeySpec

instance NFData DataKeySpec

instance ToByteString DataKeySpec

instance ToQuery DataKeySpec

instance ToHeader DataKeySpec

instance ToJSON DataKeySpec where
  toJSON = toJSONText

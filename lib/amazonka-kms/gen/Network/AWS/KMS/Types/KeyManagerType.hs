{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.KeyManagerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.KeyManagerType where

import Network.AWS.Prelude

data KeyManagerType
  = AWS
  | Customer
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

instance FromText KeyManagerType where
  parser =
    takeLowerText >>= \case
      "aws" -> pure AWS
      "customer" -> pure Customer
      e ->
        fromTextError $
          "Failure parsing KeyManagerType from value: '" <> e
            <> "'. Accepted values: aws, customer"

instance ToText KeyManagerType where
  toText = \case
    AWS -> "AWS"
    Customer -> "CUSTOMER"

instance Hashable KeyManagerType

instance NFData KeyManagerType

instance ToByteString KeyManagerType

instance ToQuery KeyManagerType

instance ToHeader KeyManagerType

instance FromJSON KeyManagerType where
  parseJSON = parseJSONText "KeyManagerType"

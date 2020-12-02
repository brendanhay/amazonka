{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.KeyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.KeyType where

import Network.AWS.Prelude

data KeyType
  = AWSOwnedCmk
  | CustomerManagedCmk
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

instance FromText KeyType where
  parser =
    takeLowerText >>= \case
      "aws_owned_cmk" -> pure AWSOwnedCmk
      "customer_managed_cmk" -> pure CustomerManagedCmk
      e ->
        fromTextError $
          "Failure parsing KeyType from value: '" <> e
            <> "'. Accepted values: aws_owned_cmk, customer_managed_cmk"

instance ToText KeyType where
  toText = \case
    AWSOwnedCmk -> "AWS_OWNED_CMK"
    CustomerManagedCmk -> "CUSTOMER_MANAGED_CMK"

instance Hashable KeyType

instance NFData KeyType

instance ToByteString KeyType

instance ToQuery KeyType

instance ToHeader KeyType

instance ToJSON KeyType where
  toJSON = toJSONText

instance FromJSON KeyType where
  parseJSON = parseJSONText "KeyType"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.DomainType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DomainType where

import Network.AWS.Prelude

data DomainType
  = AWSManaged
  | CustomerManaged
  | Endpoint
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

instance FromText DomainType where
  parser =
    takeLowerText >>= \case
      "aws_managed" -> pure AWSManaged
      "customer_managed" -> pure CustomerManaged
      "endpoint" -> pure Endpoint
      e ->
        fromTextError $
          "Failure parsing DomainType from value: '" <> e
            <> "'. Accepted values: aws_managed, customer_managed, endpoint"

instance ToText DomainType where
  toText = \case
    AWSManaged -> "AWS_MANAGED"
    CustomerManaged -> "CUSTOMER_MANAGED"
    Endpoint -> "ENDPOINT"

instance Hashable DomainType

instance NFData DomainType

instance ToByteString DomainType

instance ToQuery DomainType

instance ToHeader DomainType

instance FromJSON DomainType where
  parseJSON = parseJSONText "DomainType"

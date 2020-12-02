{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.RequestCharged
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.RequestCharged where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | If present, indicates that the requester was successfully charged for the request.
data RequestCharged = RCRequester
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

instance FromText RequestCharged where
  parser =
    takeLowerText >>= \case
      "requester" -> pure RCRequester
      e ->
        fromTextError $
          "Failure parsing RequestCharged from value: '" <> e
            <> "'. Accepted values: requester"

instance ToText RequestCharged where
  toText = \case
    RCRequester -> "requester"

instance Hashable RequestCharged

instance NFData RequestCharged

instance ToByteString RequestCharged

instance ToQuery RequestCharged

instance ToHeader RequestCharged

instance FromXML RequestCharged where
  parseXML = parseXMLText "RequestCharged"

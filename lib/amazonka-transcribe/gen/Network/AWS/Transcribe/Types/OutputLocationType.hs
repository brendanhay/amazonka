{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.OutputLocationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.OutputLocationType where

import Network.AWS.Prelude

data OutputLocationType
  = CustomerBucket
  | ServiceBucket
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

instance FromText OutputLocationType where
  parser =
    takeLowerText >>= \case
      "customer_bucket" -> pure CustomerBucket
      "service_bucket" -> pure ServiceBucket
      e ->
        fromTextError $
          "Failure parsing OutputLocationType from value: '" <> e
            <> "'. Accepted values: customer_bucket, service_bucket"

instance ToText OutputLocationType where
  toText = \case
    CustomerBucket -> "CUSTOMER_BUCKET"
    ServiceBucket -> "SERVICE_BUCKET"

instance Hashable OutputLocationType

instance NFData OutputLocationType

instance ToByteString OutputLocationType

instance ToQuery OutputLocationType

instance ToHeader OutputLocationType

instance FromJSON OutputLocationType where
  parseJSON = parseJSONText "OutputLocationType"

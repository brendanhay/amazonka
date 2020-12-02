{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DeliveryStreamType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.DeliveryStreamType where

import Network.AWS.Prelude

data DeliveryStreamType
  = DirectPut
  | KinesisStreamAsSource
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

instance FromText DeliveryStreamType where
  parser =
    takeLowerText >>= \case
      "directput" -> pure DirectPut
      "kinesisstreamassource" -> pure KinesisStreamAsSource
      e ->
        fromTextError $
          "Failure parsing DeliveryStreamType from value: '" <> e
            <> "'. Accepted values: directput, kinesisstreamassource"

instance ToText DeliveryStreamType where
  toText = \case
    DirectPut -> "DirectPut"
    KinesisStreamAsSource -> "KinesisStreamAsSource"

instance Hashable DeliveryStreamType

instance NFData DeliveryStreamType

instance ToByteString DeliveryStreamType

instance ToQuery DeliveryStreamType

instance ToHeader DeliveryStreamType

instance ToJSON DeliveryStreamType where
  toJSON = toJSONText

instance FromJSON DeliveryStreamType where
  parseJSON = parseJSONText "DeliveryStreamType"

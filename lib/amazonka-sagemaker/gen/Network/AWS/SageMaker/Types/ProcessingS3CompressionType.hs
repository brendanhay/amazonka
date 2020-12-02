{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3CompressionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingS3CompressionType where

import Network.AWS.Prelude

data ProcessingS3CompressionType
  = PSCTGzip
  | PSCTNone
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

instance FromText ProcessingS3CompressionType where
  parser =
    takeLowerText >>= \case
      "gzip" -> pure PSCTGzip
      "none" -> pure PSCTNone
      e ->
        fromTextError $
          "Failure parsing ProcessingS3CompressionType from value: '" <> e
            <> "'. Accepted values: gzip, none"

instance ToText ProcessingS3CompressionType where
  toText = \case
    PSCTGzip -> "Gzip"
    PSCTNone -> "None"

instance Hashable ProcessingS3CompressionType

instance NFData ProcessingS3CompressionType

instance ToByteString ProcessingS3CompressionType

instance ToQuery ProcessingS3CompressionType

instance ToHeader ProcessingS3CompressionType

instance ToJSON ProcessingS3CompressionType where
  toJSON = toJSONText

instance FromJSON ProcessingS3CompressionType where
  parseJSON = parseJSONText "ProcessingS3CompressionType"

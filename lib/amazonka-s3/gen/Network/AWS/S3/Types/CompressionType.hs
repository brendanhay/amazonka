{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CompressionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CompressionType where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data CompressionType
  = CTBZIP2
  | CTGzip
  | CTNone
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

instance FromText CompressionType where
  parser =
    takeLowerText >>= \case
      "bzip2" -> pure CTBZIP2
      "gzip" -> pure CTGzip
      "none" -> pure CTNone
      e ->
        fromTextError $
          "Failure parsing CompressionType from value: '" <> e
            <> "'. Accepted values: bzip2, gzip, none"

instance ToText CompressionType where
  toText = \case
    CTBZIP2 -> "BZIP2"
    CTGzip -> "GZIP"
    CTNone -> "NONE"

instance Hashable CompressionType

instance NFData CompressionType

instance ToByteString CompressionType

instance ToQuery CompressionType

instance ToHeader CompressionType

instance ToXML CompressionType where
  toXML = toXMLText

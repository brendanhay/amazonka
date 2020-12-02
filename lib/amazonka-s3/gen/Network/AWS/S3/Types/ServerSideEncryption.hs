{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ServerSideEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ServerSideEncryption where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ServerSideEncryption
  = AES256
  | AWSKMS
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

instance FromText ServerSideEncryption where
  parser =
    takeLowerText >>= \case
      "aes256" -> pure AES256
      "aws:kms" -> pure AWSKMS
      e ->
        fromTextError $
          "Failure parsing ServerSideEncryption from value: '" <> e
            <> "'. Accepted values: aes256, aws:kms"

instance ToText ServerSideEncryption where
  toText = \case
    AES256 -> "AES256"
    AWSKMS -> "aws:kms"

instance Hashable ServerSideEncryption

instance NFData ServerSideEncryption

instance ToByteString ServerSideEncryption

instance ToQuery ServerSideEncryption

instance ToHeader ServerSideEncryption

instance FromXML ServerSideEncryption where
  parseXML = parseXMLText "ServerSideEncryption"

instance ToXML ServerSideEncryption where
  toXML = toXMLText

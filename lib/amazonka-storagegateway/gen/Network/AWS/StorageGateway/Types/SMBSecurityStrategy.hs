{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.SMBSecurityStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.SMBSecurityStrategy where

import Network.AWS.Prelude

data SMBSecurityStrategy
  = SMBSSClientSpecified
  | SMBSSMandatoryEncryption
  | SMBSSMandatorySigning
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

instance FromText SMBSecurityStrategy where
  parser =
    takeLowerText >>= \case
      "clientspecified" -> pure SMBSSClientSpecified
      "mandatoryencryption" -> pure SMBSSMandatoryEncryption
      "mandatorysigning" -> pure SMBSSMandatorySigning
      e ->
        fromTextError $
          "Failure parsing SMBSecurityStrategy from value: '" <> e
            <> "'. Accepted values: clientspecified, mandatoryencryption, mandatorysigning"

instance ToText SMBSecurityStrategy where
  toText = \case
    SMBSSClientSpecified -> "ClientSpecified"
    SMBSSMandatoryEncryption -> "MandatoryEncryption"
    SMBSSMandatorySigning -> "MandatorySigning"

instance Hashable SMBSecurityStrategy

instance NFData SMBSecurityStrategy

instance ToByteString SMBSecurityStrategy

instance ToQuery SMBSecurityStrategy

instance ToHeader SMBSecurityStrategy

instance ToJSON SMBSecurityStrategy where
  toJSON = toJSONText

instance FromJSON SMBSecurityStrategy where
  parseJSON = parseJSONText "SMBSecurityStrategy"

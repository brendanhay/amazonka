{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.NetworkInputServerValidation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.NetworkInputServerValidation where

import Network.AWS.Prelude

-- | Network Input Server Validation
data NetworkInputServerValidation
  = CheckCryptographyAndValidateName
  | CheckCryptographyOnly
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

instance FromText NetworkInputServerValidation where
  parser =
    takeLowerText >>= \case
      "check_cryptography_and_validate_name" -> pure CheckCryptographyAndValidateName
      "check_cryptography_only" -> pure CheckCryptographyOnly
      e ->
        fromTextError $
          "Failure parsing NetworkInputServerValidation from value: '" <> e
            <> "'. Accepted values: check_cryptography_and_validate_name, check_cryptography_only"

instance ToText NetworkInputServerValidation where
  toText = \case
    CheckCryptographyAndValidateName -> "CHECK_CRYPTOGRAPHY_AND_VALIDATE_NAME"
    CheckCryptographyOnly -> "CHECK_CRYPTOGRAPHY_ONLY"

instance Hashable NetworkInputServerValidation

instance NFData NetworkInputServerValidation

instance ToByteString NetworkInputServerValidation

instance ToQuery NetworkInputServerValidation

instance ToHeader NetworkInputServerValidation

instance ToJSON NetworkInputServerValidation where
  toJSON = toJSONText

instance FromJSON NetworkInputServerValidation where
  parseJSON = parseJSONText "NetworkInputServerValidation"

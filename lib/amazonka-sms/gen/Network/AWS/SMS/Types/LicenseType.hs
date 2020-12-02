{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.LicenseType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.LicenseType where

import Network.AWS.Prelude

data LicenseType
  = AWS
  | Byol
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

instance FromText LicenseType where
  parser =
    takeLowerText >>= \case
      "aws" -> pure AWS
      "byol" -> pure Byol
      e ->
        fromTextError $
          "Failure parsing LicenseType from value: '" <> e
            <> "'. Accepted values: aws, byol"

instance ToText LicenseType where
  toText = \case
    AWS -> "AWS"
    Byol -> "BYOL"

instance Hashable LicenseType

instance NFData LicenseType

instance ToByteString LicenseType

instance ToQuery LicenseType

instance ToHeader LicenseType

instance ToJSON LicenseType where
  toJSON = toJSONText

instance FromJSON LicenseType where
  parseJSON = parseJSONText "LicenseType"

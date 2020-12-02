{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AccountAttributeName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AccountAttributeName where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data AccountAttributeName
  = DefaultVPC
  | SupportedPlatforms
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

instance FromText AccountAttributeName where
  parser =
    takeLowerText >>= \case
      "default-vpc" -> pure DefaultVPC
      "supported-platforms" -> pure SupportedPlatforms
      e ->
        fromTextError $
          "Failure parsing AccountAttributeName from value: '" <> e
            <> "'. Accepted values: default-vpc, supported-platforms"

instance ToText AccountAttributeName where
  toText = \case
    DefaultVPC -> "default-vpc"
    SupportedPlatforms -> "supported-platforms"

instance Hashable AccountAttributeName

instance NFData AccountAttributeName

instance ToByteString AccountAttributeName

instance ToQuery AccountAttributeName

instance ToHeader AccountAttributeName

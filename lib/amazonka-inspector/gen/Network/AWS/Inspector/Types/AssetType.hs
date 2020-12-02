{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssetType where

import Network.AWS.Prelude

data AssetType = EC2Instance
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

instance FromText AssetType where
  parser =
    takeLowerText >>= \case
      "ec2-instance" -> pure EC2Instance
      e ->
        fromTextError $
          "Failure parsing AssetType from value: '" <> e
            <> "'. Accepted values: ec2-instance"

instance ToText AssetType where
  toText = \case
    EC2Instance -> "ec2-instance"

instance Hashable AssetType

instance NFData AssetType

instance ToByteString AssetType

instance ToQuery AssetType

instance ToHeader AssetType

instance FromJSON AssetType where
  parseJSON = parseJSONText "AssetType"

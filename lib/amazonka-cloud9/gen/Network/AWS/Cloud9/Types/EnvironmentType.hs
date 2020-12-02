{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.Types.EnvironmentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Cloud9.Types.EnvironmentType where

import Network.AWS.Prelude

data EnvironmentType
  = EC2
  | SSH
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

instance FromText EnvironmentType where
  parser =
    takeLowerText >>= \case
      "ec2" -> pure EC2
      "ssh" -> pure SSH
      e ->
        fromTextError $
          "Failure parsing EnvironmentType from value: '" <> e
            <> "'. Accepted values: ec2, ssh"

instance ToText EnvironmentType where
  toText = \case
    EC2 -> "ec2"
    SSH -> "ssh"

instance Hashable EnvironmentType

instance NFData EnvironmentType

instance ToByteString EnvironmentType

instance ToQuery EnvironmentType

instance ToHeader EnvironmentType

instance FromJSON EnvironmentType where
  parseJSON = parseJSONText "EnvironmentType"

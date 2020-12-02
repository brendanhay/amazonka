{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceType where

import Network.AWS.Prelude

data ResourceType
  = Document
  | EC2Instance
  | ManagedInstance
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

instance FromText ResourceType where
  parser =
    takeLowerText >>= \case
      "document" -> pure Document
      "ec2instance" -> pure EC2Instance
      "managedinstance" -> pure ManagedInstance
      e ->
        fromTextError $
          "Failure parsing ResourceType from value: '" <> e
            <> "'. Accepted values: document, ec2instance, managedinstance"

instance ToText ResourceType where
  toText = \case
    Document -> "Document"
    EC2Instance -> "EC2Instance"
    ManagedInstance -> "ManagedInstance"

instance Hashable ResourceType

instance NFData ResourceType

instance ToByteString ResourceType

instance ToQuery ResourceType

instance ToHeader ResourceType

instance FromJSON ResourceType where
  parseJSON = parseJSONText "ResourceType"

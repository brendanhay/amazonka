{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.OperationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.OperationType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data OperationType
  = Add
  | Remove
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

instance FromText OperationType where
  parser =
    takeLowerText >>= \case
      "add" -> pure Add
      "remove" -> pure Remove
      e ->
        fromTextError $
          "Failure parsing OperationType from value: '" <> e
            <> "'. Accepted values: add, remove"

instance ToText OperationType where
  toText = \case
    Add -> "add"
    Remove -> "remove"

instance Hashable OperationType

instance NFData OperationType

instance ToByteString OperationType

instance ToQuery OperationType

instance ToHeader OperationType

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Replacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Replacement where

import Network.AWS.Prelude

data Replacement
  = Conditional
  | False'
  | True'
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

instance FromText Replacement where
  parser =
    takeLowerText >>= \case
      "conditional" -> pure Conditional
      "false" -> pure False'
      "true" -> pure True'
      e ->
        fromTextError $
          "Failure parsing Replacement from value: '" <> e
            <> "'. Accepted values: conditional, false, true"

instance ToText Replacement where
  toText = \case
    Conditional -> "Conditional"
    False' -> "False"
    True' -> "True"

instance Hashable Replacement

instance NFData Replacement

instance ToByteString Replacement

instance ToQuery Replacement

instance ToHeader Replacement

instance FromXML Replacement where
  parseXML = parseXMLText "Replacement"

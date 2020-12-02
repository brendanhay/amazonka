{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Visibility
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Visibility where

import Network.AWS.Prelude

data Visibility
  = Private
  | Public
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

instance FromText Visibility where
  parser =
    takeLowerText >>= \case
      "private" -> pure Private
      "public" -> pure Public
      e ->
        fromTextError $
          "Failure parsing Visibility from value: '" <> e
            <> "'. Accepted values: private, public"

instance ToText Visibility where
  toText = \case
    Private -> "PRIVATE"
    Public -> "PUBLIC"

instance Hashable Visibility

instance NFData Visibility

instance ToByteString Visibility

instance ToQuery Visibility

instance ToHeader Visibility

instance FromXML Visibility where
  parseXML = parseXMLText "Visibility"

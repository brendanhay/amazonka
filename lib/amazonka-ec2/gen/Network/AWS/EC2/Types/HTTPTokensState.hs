{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HTTPTokensState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HTTPTokensState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data HTTPTokensState
  = HTTPTSOptional
  | HTTPTSRequired
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

instance FromText HTTPTokensState where
  parser =
    takeLowerText >>= \case
      "optional" -> pure HTTPTSOptional
      "required" -> pure HTTPTSRequired
      e ->
        fromTextError $
          "Failure parsing HTTPTokensState from value: '" <> e
            <> "'. Accepted values: optional, required"

instance ToText HTTPTokensState where
  toText = \case
    HTTPTSOptional -> "optional"
    HTTPTSRequired -> "required"

instance Hashable HTTPTokensState

instance NFData HTTPTokensState

instance ToByteString HTTPTokensState

instance ToQuery HTTPTokensState

instance ToHeader HTTPTokensState

instance FromXML HTTPTokensState where
  parseXML = parseXMLText "HTTPTokensState"

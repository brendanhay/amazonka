{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.AccountJoinedMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.AccountJoinedMethod where

import Network.AWS.Prelude

data AccountJoinedMethod
  = Created
  | Invited
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

instance FromText AccountJoinedMethod where
  parser =
    takeLowerText >>= \case
      "created" -> pure Created
      "invited" -> pure Invited
      e ->
        fromTextError $
          "Failure parsing AccountJoinedMethod from value: '" <> e
            <> "'. Accepted values: created, invited"

instance ToText AccountJoinedMethod where
  toText = \case
    Created -> "CREATED"
    Invited -> "INVITED"

instance Hashable AccountJoinedMethod

instance NFData AccountJoinedMethod

instance ToByteString AccountJoinedMethod

instance ToQuery AccountJoinedMethod

instance ToHeader AccountJoinedMethod

instance FromJSON AccountJoinedMethod where
  parseJSON = parseJSONText "AccountJoinedMethod"

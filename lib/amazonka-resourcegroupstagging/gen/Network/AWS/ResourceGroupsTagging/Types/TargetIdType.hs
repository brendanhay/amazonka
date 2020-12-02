{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.TargetIdType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.TargetIdType where

import Network.AWS.Prelude

data TargetIdType
  = Account
  | OU
  | Root
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

instance FromText TargetIdType where
  parser =
    takeLowerText >>= \case
      "account" -> pure Account
      "ou" -> pure OU
      "root" -> pure Root
      e ->
        fromTextError $
          "Failure parsing TargetIdType from value: '" <> e
            <> "'. Accepted values: account, ou, root"

instance ToText TargetIdType where
  toText = \case
    Account -> "ACCOUNT"
    OU -> "OU"
    Root -> "ROOT"

instance Hashable TargetIdType

instance NFData TargetIdType

instance ToByteString TargetIdType

instance ToQuery TargetIdType

instance ToHeader TargetIdType

instance FromJSON TargetIdType where
  parseJSON = parseJSONText "TargetIdType"

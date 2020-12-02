{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.ResourceCollectionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.ResourceCollectionType where

import Network.AWS.Prelude

data ResourceCollectionType = SharedWithMe
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

instance FromText ResourceCollectionType where
  parser =
    takeLowerText >>= \case
      "shared_with_me" -> pure SharedWithMe
      e ->
        fromTextError $
          "Failure parsing ResourceCollectionType from value: '" <> e
            <> "'. Accepted values: shared_with_me"

instance ToText ResourceCollectionType where
  toText = \case
    SharedWithMe -> "SHARED_WITH_ME"

instance Hashable ResourceCollectionType

instance NFData ResourceCollectionType

instance ToByteString ResourceCollectionType

instance ToQuery ResourceCollectionType

instance ToHeader ResourceCollectionType

instance ToJSON ResourceCollectionType where
  toJSON = toJSONText

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UserProfileSortKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UserProfileSortKey where

import Network.AWS.Prelude

data UserProfileSortKey
  = UPSKCreationTime
  | UPSKLastModifiedTime
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

instance FromText UserProfileSortKey where
  parser =
    takeLowerText >>= \case
      "creationtime" -> pure UPSKCreationTime
      "lastmodifiedtime" -> pure UPSKLastModifiedTime
      e ->
        fromTextError $
          "Failure parsing UserProfileSortKey from value: '" <> e
            <> "'. Accepted values: creationtime, lastmodifiedtime"

instance ToText UserProfileSortKey where
  toText = \case
    UPSKCreationTime -> "CreationTime"
    UPSKLastModifiedTime -> "LastModifiedTime"

instance Hashable UserProfileSortKey

instance NFData UserProfileSortKey

instance ToByteString UserProfileSortKey

instance ToQuery UserProfileSortKey

instance ToHeader UserProfileSortKey

instance ToJSON UserProfileSortKey where
  toJSON = toJSONText

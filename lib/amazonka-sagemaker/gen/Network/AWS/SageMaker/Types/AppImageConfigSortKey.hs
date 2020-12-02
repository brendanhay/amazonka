{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AppImageConfigSortKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AppImageConfigSortKey where

import Network.AWS.Prelude

data AppImageConfigSortKey
  = AICSKCreationTime
  | AICSKLastModifiedTime
  | AICSKName
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

instance FromText AppImageConfigSortKey where
  parser =
    takeLowerText >>= \case
      "creationtime" -> pure AICSKCreationTime
      "lastmodifiedtime" -> pure AICSKLastModifiedTime
      "name" -> pure AICSKName
      e ->
        fromTextError $
          "Failure parsing AppImageConfigSortKey from value: '" <> e
            <> "'. Accepted values: creationtime, lastmodifiedtime, name"

instance ToText AppImageConfigSortKey where
  toText = \case
    AICSKCreationTime -> "CreationTime"
    AICSKLastModifiedTime -> "LastModifiedTime"
    AICSKName -> "Name"

instance Hashable AppImageConfigSortKey

instance NFData AppImageConfigSortKey

instance ToByteString AppImageConfigSortKey

instance ToQuery AppImageConfigSortKey

instance ToHeader AppImageConfigSortKey

instance ToJSON AppImageConfigSortKey where
  toJSON = toJSONText

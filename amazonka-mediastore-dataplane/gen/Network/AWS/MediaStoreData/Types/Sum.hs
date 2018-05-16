{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStoreData.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaStoreData.Types.Sum where

import Network.AWS.Prelude

data ItemType
  = TypeFolder
  | TypeObject
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ItemType where
    parser = takeLowerText >>= \case
        "folder" -> pure TypeFolder
        "object" -> pure TypeObject
        e -> fromTextError $ "Failure parsing ItemType from value: '" <> e
           <> "'. Accepted values: folder, object"

instance ToText ItemType where
    toText = \case
        TypeFolder -> "FOLDER"
        TypeObject -> "OBJECT"

instance Hashable     ItemType
instance NFData       ItemType
instance ToByteString ItemType
instance ToQuery      ItemType
instance ToHeader     ItemType

instance FromJSON ItemType where
    parseJSON = parseJSONText "ItemType"

data StorageClass =
  Temporal
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StorageClass where
    parser = takeLowerText >>= \case
        "temporal" -> pure Temporal
        e -> fromTextError $ "Failure parsing StorageClass from value: '" <> e
           <> "'. Accepted values: temporal"

instance ToText StorageClass where
    toText = \case
        Temporal -> "TEMPORAL"

instance Hashable     StorageClass
instance NFData       StorageClass
instance ToByteString StorageClass
instance ToQuery      StorageClass
instance ToHeader     StorageClass

instance ToJSON StorageClass where
    toJSON = toJSONText

instance FromJSON StorageClass where
    parseJSON = parseJSONText "StorageClass"

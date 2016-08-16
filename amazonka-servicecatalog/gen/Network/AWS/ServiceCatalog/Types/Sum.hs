{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.Sum where

import           Network.AWS.Prelude

data ProductViewFilterBy
    = FullTextSearch
    | Owner
    | ProductType
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ProductViewFilterBy where
    parser = takeLowerText >>= \case
        "fulltextsearch" -> pure FullTextSearch
        "owner" -> pure Owner
        "producttype" -> pure ProductType
        e -> fromTextError $ "Failure parsing ProductViewFilterBy from value: '" <> e
           <> "'. Accepted values: FullTextSearch, Owner, ProductType"

instance ToText ProductViewFilterBy where
    toText = \case
        FullTextSearch -> "FullTextSearch"
        Owner -> "Owner"
        ProductType -> "ProductType"

instance Hashable     ProductViewFilterBy
instance NFData       ProductViewFilterBy
instance ToByteString ProductViewFilterBy
instance ToQuery      ProductViewFilterBy
instance ToHeader     ProductViewFilterBy

instance ToJSON ProductViewFilterBy where
    toJSON = toJSONText

data ProductViewSortBy
    = CreationDate
    | Title
    | VersionCount
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ProductViewSortBy where
    parser = takeLowerText >>= \case
        "creationdate" -> pure CreationDate
        "title" -> pure Title
        "versioncount" -> pure VersionCount
        e -> fromTextError $ "Failure parsing ProductViewSortBy from value: '" <> e
           <> "'. Accepted values: CreationDate, Title, VersionCount"

instance ToText ProductViewSortBy where
    toText = \case
        CreationDate -> "CreationDate"
        Title -> "Title"
        VersionCount -> "VersionCount"

instance Hashable     ProductViewSortBy
instance NFData       ProductViewSortBy
instance ToByteString ProductViewSortBy
instance ToQuery      ProductViewSortBy
instance ToHeader     ProductViewSortBy

instance ToJSON ProductViewSortBy where
    toJSON = toJSONText

data RecordStatus
    = Error'
    | InProgress
    | Succeeded
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText RecordStatus where
    parser = takeLowerText >>= \case
        "error" -> pure Error'
        "in_progress" -> pure InProgress
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing RecordStatus from value: '" <> e
           <> "'. Accepted values: ERROR, IN_PROGRESS, SUCCEEDED"

instance ToText RecordStatus where
    toText = \case
        Error' -> "ERROR"
        InProgress -> "IN_PROGRESS"
        Succeeded -> "SUCCEEDED"

instance Hashable     RecordStatus
instance NFData       RecordStatus
instance ToByteString RecordStatus
instance ToQuery      RecordStatus
instance ToHeader     RecordStatus

instance FromJSON RecordStatus where
    parseJSON = parseJSONText "RecordStatus"

data SortOrder
    = Ascending
    | Descending
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText SortOrder where
    parser = takeLowerText >>= \case
        "ascending" -> pure Ascending
        "descending" -> pure Descending
        e -> fromTextError $ "Failure parsing SortOrder from value: '" <> e
           <> "'. Accepted values: ASCENDING, DESCENDING"

instance ToText SortOrder where
    toText = \case
        Ascending -> "ASCENDING"
        Descending -> "DESCENDING"

instance Hashable     SortOrder
instance NFData       SortOrder
instance ToByteString SortOrder
instance ToQuery      SortOrder
instance ToHeader     SortOrder

instance ToJSON SortOrder where
    toJSON = toJSONText

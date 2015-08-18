{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.Sum where

import           Network.AWS.Prelude

data ActionCode
    = ArchiveRetrieval
    | InventoryRetrieval
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ActionCode where
    parser = takeLowerText >>= \case
        "archiveretrieval" -> pure ArchiveRetrieval
        "inventoryretrieval" -> pure InventoryRetrieval
        e -> fromTextError $ "Failure parsing ActionCode from value: '" <> e
           <> "'. Accepted values: ArchiveRetrieval, InventoryRetrieval"

instance ToText ActionCode where
    toText = \case
        ArchiveRetrieval -> "ArchiveRetrieval"
        InventoryRetrieval -> "InventoryRetrieval"

instance Hashable     ActionCode
instance ToByteString ActionCode
instance ToQuery      ActionCode
instance ToHeader     ActionCode

instance FromJSON ActionCode where
    parseJSON = parseJSONText "ActionCode"

data StatusCode
    = Failed
    | InProgress
    | Succeeded
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StatusCode where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "inprogress" -> pure InProgress
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing StatusCode from value: '" <> e
           <> "'. Accepted values: Failed, InProgress, Succeeded"

instance ToText StatusCode where
    toText = \case
        Failed -> "Failed"
        InProgress -> "InProgress"
        Succeeded -> "Succeeded"

instance Hashable     StatusCode
instance ToByteString StatusCode
instance ToQuery      StatusCode
instance ToHeader     StatusCode

instance FromJSON StatusCode where
    parseJSON = parseJSONText "StatusCode"

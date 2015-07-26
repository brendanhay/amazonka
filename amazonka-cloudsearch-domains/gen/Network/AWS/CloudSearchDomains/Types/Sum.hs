{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearchDomains.Types.Sum where

import           Network.AWS.Prelude

data ContentType
    = ApplicationJSON
    | ApplicationXML
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ContentType where
    parser = takeLowerText >>= \case
        "application/json" -> pure ApplicationJSON
        "application/xml" -> pure ApplicationXML
        e -> fromTextError $ "Failure parsing ContentType from value: '" <> e
           <> "'. Accepted values: application/json, application/xml"

instance ToText ContentType where
    toText = \case
        ApplicationJSON -> "application/json"
        ApplicationXML -> "application/xml"

instance Hashable     ContentType
instance ToByteString ContentType
instance ToPath       ContentType
instance ToQuery      ContentType
instance ToHeader     ContentType

instance ToJSON ContentType where
    toJSON = toJSONText

data QueryParser
    = Lucene
    | Dismax
    | Simple
    | Structured
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText QueryParser where
    parser = takeLowerText >>= \case
        "dismax" -> pure Dismax
        "lucene" -> pure Lucene
        "simple" -> pure Simple
        "structured" -> pure Structured
        e -> fromTextError $ "Failure parsing QueryParser from value: '" <> e
           <> "'. Accepted values: dismax, lucene, simple, structured"

instance ToText QueryParser where
    toText = \case
        Dismax -> "dismax"
        Lucene -> "lucene"
        Simple -> "simple"
        Structured -> "structured"

instance Hashable     QueryParser
instance ToByteString QueryParser
instance ToPath       QueryParser
instance ToQuery      QueryParser
instance ToHeader     QueryParser

instance ToJSON QueryParser where
    toJSON = toJSONText

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.APIGateway.Types.Sum where

import           Network.AWS.Prelude

data AuthorizerType =
    Token
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText AuthorizerType where
    parser = takeLowerText >>= \case
        "token" -> pure Token
        e -> fromTextError $ "Failure parsing AuthorizerType from value: '" <> e
           <> "'. Accepted values: TOKEN"

instance ToText AuthorizerType where
    toText = \case
        Token -> "TOKEN"

instance Hashable     AuthorizerType
instance ToByteString AuthorizerType
instance ToQuery      AuthorizerType
instance ToHeader     AuthorizerType

instance ToJSON AuthorizerType where
    toJSON = toJSONText

instance FromJSON AuthorizerType where
    parseJSON = parseJSONText "AuthorizerType"

-- | Returns the size of the __CacheCluster__.
data CacheClusterSize
    = D0_5
    | D118
    | D13_5
    | D1_6
    | D237
    | D28_4
    | D58_2
    | D6_1
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText CacheClusterSize where
    parser = takeLowerText >>= \case
        "0.5" -> pure D0_5
        "118" -> pure D118
        "13.5" -> pure D13_5
        "1.6" -> pure D1_6
        "237" -> pure D237
        "28.4" -> pure D28_4
        "58.2" -> pure D58_2
        "6.1" -> pure D6_1
        e -> fromTextError $ "Failure parsing CacheClusterSize from value: '" <> e
           <> "'. Accepted values: 0.5, 118, 13.5, 1.6, 237, 28.4, 58.2, 6.1"

instance ToText CacheClusterSize where
    toText = \case
        D0_5 -> "0.5"
        D118 -> "118"
        D13_5 -> "13.5"
        D1_6 -> "1.6"
        D237 -> "237"
        D28_4 -> "28.4"
        D58_2 -> "58.2"
        D6_1 -> "6.1"

instance Hashable     CacheClusterSize
instance ToByteString CacheClusterSize
instance ToQuery      CacheClusterSize
instance ToHeader     CacheClusterSize

instance ToJSON CacheClusterSize where
    toJSON = toJSONText

instance FromJSON CacheClusterSize where
    parseJSON = parseJSONText "CacheClusterSize"

-- | Returns the status of the __CacheCluster__.
data CacheClusterStatus
    = Available
    | CreateInProgress
    | DeleteInProgress
    | FlushInProgress
    | NotAvailable
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText CacheClusterStatus where
    parser = takeLowerText >>= \case
        "available" -> pure Available
        "create_in_progress" -> pure CreateInProgress
        "delete_in_progress" -> pure DeleteInProgress
        "flush_in_progress" -> pure FlushInProgress
        "not_available" -> pure NotAvailable
        e -> fromTextError $ "Failure parsing CacheClusterStatus from value: '" <> e
           <> "'. Accepted values: AVAILABLE, CREATE_IN_PROGRESS, DELETE_IN_PROGRESS, FLUSH_IN_PROGRESS, NOT_AVAILABLE"

instance ToText CacheClusterStatus where
    toText = \case
        Available -> "AVAILABLE"
        CreateInProgress -> "CREATE_IN_PROGRESS"
        DeleteInProgress -> "DELETE_IN_PROGRESS"
        FlushInProgress -> "FLUSH_IN_PROGRESS"
        NotAvailable -> "NOT_AVAILABLE"

instance Hashable     CacheClusterStatus
instance ToByteString CacheClusterStatus
instance ToQuery      CacheClusterStatus
instance ToHeader     CacheClusterStatus

instance FromJSON CacheClusterStatus where
    parseJSON = parseJSONText "CacheClusterStatus"

-- | The integration type. Possible values are HTTP, AWS, or Mock.
data IntegrationType
    = AWS
    | HTTP
    | Mock
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText IntegrationType where
    parser = takeLowerText >>= \case
        "aws" -> pure AWS
        "http" -> pure HTTP
        "mock" -> pure Mock
        e -> fromTextError $ "Failure parsing IntegrationType from value: '" <> e
           <> "'. Accepted values: AWS, HTTP, MOCK"

instance ToText IntegrationType where
    toText = \case
        AWS -> "AWS"
        HTTP -> "HTTP"
        Mock -> "MOCK"

instance Hashable     IntegrationType
instance ToByteString IntegrationType
instance ToQuery      IntegrationType
instance ToHeader     IntegrationType

instance ToJSON IntegrationType where
    toJSON = toJSONText

instance FromJSON IntegrationType where
    parseJSON = parseJSONText "IntegrationType"

data Op
    = Add
    | Copy
    | Move
    | Remove
    | Replace
    | Test
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText Op where
    parser = takeLowerText >>= \case
        "add" -> pure Add
        "copy" -> pure Copy
        "move" -> pure Move
        "remove" -> pure Remove
        "replace" -> pure Replace
        "test" -> pure Test
        e -> fromTextError $ "Failure parsing Op from value: '" <> e
           <> "'. Accepted values: add, copy, move, remove, replace, test"

instance ToText Op where
    toText = \case
        Add -> "add"
        Copy -> "copy"
        Move -> "move"
        Remove -> "remove"
        Replace -> "replace"
        Test -> "test"

instance Hashable     Op
instance ToByteString Op
instance ToQuery      Op
instance ToHeader     Op

instance ToJSON Op where
    toJSON = toJSONText

data UnauthorizedCacheControlHeaderStrategy
    = FailWith403
    | SucceedWithResponseHeader
    | SucceedWithoutResponseHeader
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText UnauthorizedCacheControlHeaderStrategy where
    parser = takeLowerText >>= \case
        "fail_with_403" -> pure FailWith403
        "succeed_with_response_header" -> pure SucceedWithResponseHeader
        "succeed_without_response_header" -> pure SucceedWithoutResponseHeader
        e -> fromTextError $ "Failure parsing UnauthorizedCacheControlHeaderStrategy from value: '" <> e
           <> "'. Accepted values: FAIL_WITH_403, SUCCEED_WITH_RESPONSE_HEADER, SUCCEED_WITHOUT_RESPONSE_HEADER"

instance ToText UnauthorizedCacheControlHeaderStrategy where
    toText = \case
        FailWith403 -> "FAIL_WITH_403"
        SucceedWithResponseHeader -> "SUCCEED_WITH_RESPONSE_HEADER"
        SucceedWithoutResponseHeader -> "SUCCEED_WITHOUT_RESPONSE_HEADER"

instance Hashable     UnauthorizedCacheControlHeaderStrategy
instance ToByteString UnauthorizedCacheControlHeaderStrategy
instance ToQuery      UnauthorizedCacheControlHeaderStrategy
instance ToHeader     UnauthorizedCacheControlHeaderStrategy

instance FromJSON UnauthorizedCacheControlHeaderStrategy where
    parseJSON = parseJSONText "UnauthorizedCacheControlHeaderStrategy"

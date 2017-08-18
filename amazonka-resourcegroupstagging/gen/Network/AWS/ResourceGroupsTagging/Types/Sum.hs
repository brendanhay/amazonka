{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ResourceGroupsTagging.Types.Sum where

import           Network.AWS.Prelude

data ErrorCode
    = InternalServiceException
    | InvalidParameterException
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ErrorCode where
    parser = takeLowerText >>= \case
        "internalserviceexception" -> pure InternalServiceException
        "invalidparameterexception" -> pure InvalidParameterException
        e -> fromTextError $ "Failure parsing ErrorCode from value: '" <> e
           <> "'. Accepted values: internalserviceexception, invalidparameterexception"

instance ToText ErrorCode where
    toText = \case
        InternalServiceException -> "InternalServiceException"
        InvalidParameterException -> "InvalidParameterException"

instance Hashable     ErrorCode
instance NFData       ErrorCode
instance ToByteString ErrorCode
instance ToQuery      ErrorCode
instance ToHeader     ErrorCode

instance FromJSON ErrorCode where
    parseJSON = parseJSONText "ErrorCode"

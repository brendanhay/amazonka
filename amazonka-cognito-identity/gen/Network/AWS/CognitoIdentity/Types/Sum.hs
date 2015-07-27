{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentity.Types.Sum where

import           Network.AWS.Prelude

data CognitoErrorCode
    = InternalServerError
    | AccessDenied
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText CognitoErrorCode where
    parser = takeLowerText >>= \case
        "accessdenied" -> pure AccessDenied
        "internalservererror" -> pure InternalServerError
        e -> fromTextError $ "Failure parsing CognitoErrorCode from value: '" <> e
           <> "'. Accepted values: accessdenied, internalservererror"

instance ToText CognitoErrorCode where
    toText = \case
        AccessDenied -> "accessdenied"
        InternalServerError -> "internalservererror"

instance Hashable     CognitoErrorCode
instance ToByteString CognitoErrorCode
instance ToPath       CognitoErrorCode
instance ToQuery      CognitoErrorCode
instance ToHeader     CognitoErrorCode

instance FromJSON CognitoErrorCode where
    parseJSON = parseJSONText "CognitoErrorCode"

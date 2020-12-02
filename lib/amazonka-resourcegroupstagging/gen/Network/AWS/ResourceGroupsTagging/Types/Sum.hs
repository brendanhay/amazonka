{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ResourceGroupsTagging.Types.Sum where

import Network.AWS.Prelude

data ResourceErrorCode
  = InternalServiceException
  | InvalidParameterException
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResourceErrorCode where
    parser = takeLowerText >>= \case
        "internalserviceexception" -> pure InternalServiceException
        "invalidparameterexception" -> pure InvalidParameterException
        e -> fromTextError $ "Failure parsing ResourceErrorCode from value: '" <> e
           <> "'. Accepted values: internalserviceexception, invalidparameterexception"

instance ToText ResourceErrorCode where
    toText = \case
        InternalServiceException -> "InternalServiceException"
        InvalidParameterException -> "InvalidParameterException"

instance Hashable     ResourceErrorCode
instance NFData       ResourceErrorCode
instance ToByteString ResourceErrorCode
instance ToQuery      ResourceErrorCode
instance ToHeader     ResourceErrorCode

instance FromJSON ResourceErrorCode where
    parseJSON = parseJSONText "ResourceErrorCode"

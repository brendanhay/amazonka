{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.Sum
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.Sum where

import Network.AWS.Prelude

data SubResourceType
  = IP
  | URL
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SubResourceType where
    parser = takeLowerText >>= \case
        "ip" -> pure IP
        "url" -> pure URL
        e -> fromTextError $ "Failure parsing SubResourceType from value: '" <> e
           <> "'. Accepted values: ip, url"

instance ToText SubResourceType where
    toText = \case
        IP -> "IP"
        URL -> "URL"

instance Hashable     SubResourceType
instance NFData       SubResourceType
instance ToByteString SubResourceType
instance ToQuery      SubResourceType
instance ToHeader     SubResourceType

instance FromJSON SubResourceType where
    parseJSON = parseJSONText "SubResourceType"

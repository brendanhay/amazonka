{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceEntitlement.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MarketplaceEntitlement.Types.Sum where

import Network.AWS.Prelude

data GetEntitlementFilterName
  = CustomerIdentifier
  | Dimension
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText GetEntitlementFilterName where
    parser = takeLowerText >>= \case
        "customer_identifier" -> pure CustomerIdentifier
        "dimension" -> pure Dimension
        e -> fromTextError $ "Failure parsing GetEntitlementFilterName from value: '" <> e
           <> "'. Accepted values: customer_identifier, dimension"

instance ToText GetEntitlementFilterName where
    toText = \case
        CustomerIdentifier -> "CUSTOMER_IDENTIFIER"
        Dimension -> "DIMENSION"

instance Hashable     GetEntitlementFilterName
instance NFData       GetEntitlementFilterName
instance ToByteString GetEntitlementFilterName
instance ToQuery      GetEntitlementFilterName
instance ToHeader     GetEntitlementFilterName

instance ToJSON GetEntitlementFilterName where
    toJSON = toJSONText

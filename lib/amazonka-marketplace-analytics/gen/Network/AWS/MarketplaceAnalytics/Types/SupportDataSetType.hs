{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceAnalytics.Types.SupportDataSetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceAnalytics.Types.SupportDataSetType where

import Network.AWS.Prelude

data SupportDataSetType
  = CustomerSupportContactsData
  | TestCustomerSupportContactsData
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText SupportDataSetType where
  parser =
    takeLowerText >>= \case
      "customer_support_contacts_data" -> pure CustomerSupportContactsData
      "test_customer_support_contacts_data" -> pure TestCustomerSupportContactsData
      e ->
        fromTextError $
          "Failure parsing SupportDataSetType from value: '" <> e
            <> "'. Accepted values: customer_support_contacts_data, test_customer_support_contacts_data"

instance ToText SupportDataSetType where
  toText = \case
    CustomerSupportContactsData -> "customer_support_contacts_data"
    TestCustomerSupportContactsData -> "test_customer_support_contacts_data"

instance Hashable SupportDataSetType

instance NFData SupportDataSetType

instance ToByteString SupportDataSetType

instance ToQuery SupportDataSetType

instance ToHeader SupportDataSetType

instance ToJSON SupportDataSetType where
  toJSON = toJSONText

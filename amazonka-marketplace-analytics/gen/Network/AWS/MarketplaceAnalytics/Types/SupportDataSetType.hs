{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceAnalytics.Types.SupportDataSetType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceAnalytics.Types.SupportDataSetType
  ( SupportDataSetType
      ( ..,
        SupportDataSetType_Customer_support_contacts_data,
        SupportDataSetType_Test_customer_support_contacts_data
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype SupportDataSetType = SupportDataSetType'
  { fromSupportDataSetType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern SupportDataSetType_Customer_support_contacts_data :: SupportDataSetType
pattern SupportDataSetType_Customer_support_contacts_data = SupportDataSetType' "customer_support_contacts_data"

pattern SupportDataSetType_Test_customer_support_contacts_data :: SupportDataSetType
pattern SupportDataSetType_Test_customer_support_contacts_data = SupportDataSetType' "test_customer_support_contacts_data"

{-# COMPLETE
  SupportDataSetType_Customer_support_contacts_data,
  SupportDataSetType_Test_customer_support_contacts_data,
  SupportDataSetType'
  #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MarketplaceAnalytics.Types.SupportDataSetType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MarketplaceAnalytics.Types.SupportDataSetType
  ( SupportDataSetType
      ( ..,
        SupportDataSetType_Customer_support_contacts_data,
        SupportDataSetType_Test_customer_support_contacts_data
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SupportDataSetType = SupportDataSetType'
  { fromSupportDataSetType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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

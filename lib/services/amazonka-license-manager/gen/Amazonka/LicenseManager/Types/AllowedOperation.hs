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
-- Module      : Amazonka.LicenseManager.Types.AllowedOperation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.AllowedOperation
  ( AllowedOperation
      ( ..,
        AllowedOperation_CheckInLicense,
        AllowedOperation_CheckoutBorrowLicense,
        AllowedOperation_CheckoutLicense,
        AllowedOperation_CreateGrant,
        AllowedOperation_CreateToken,
        AllowedOperation_ExtendConsumptionLicense,
        AllowedOperation_ListPurchasedLicenses
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AllowedOperation = AllowedOperation'
  { fromAllowedOperation ::
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

pattern AllowedOperation_CheckInLicense :: AllowedOperation
pattern AllowedOperation_CheckInLicense = AllowedOperation' "CheckInLicense"

pattern AllowedOperation_CheckoutBorrowLicense :: AllowedOperation
pattern AllowedOperation_CheckoutBorrowLicense = AllowedOperation' "CheckoutBorrowLicense"

pattern AllowedOperation_CheckoutLicense :: AllowedOperation
pattern AllowedOperation_CheckoutLicense = AllowedOperation' "CheckoutLicense"

pattern AllowedOperation_CreateGrant :: AllowedOperation
pattern AllowedOperation_CreateGrant = AllowedOperation' "CreateGrant"

pattern AllowedOperation_CreateToken :: AllowedOperation
pattern AllowedOperation_CreateToken = AllowedOperation' "CreateToken"

pattern AllowedOperation_ExtendConsumptionLicense :: AllowedOperation
pattern AllowedOperation_ExtendConsumptionLicense = AllowedOperation' "ExtendConsumptionLicense"

pattern AllowedOperation_ListPurchasedLicenses :: AllowedOperation
pattern AllowedOperation_ListPurchasedLicenses = AllowedOperation' "ListPurchasedLicenses"

{-# COMPLETE
  AllowedOperation_CheckInLicense,
  AllowedOperation_CheckoutBorrowLicense,
  AllowedOperation_CheckoutLicense,
  AllowedOperation_CreateGrant,
  AllowedOperation_CreateToken,
  AllowedOperation_ExtendConsumptionLicense,
  AllowedOperation_ListPurchasedLicenses,
  AllowedOperation'
  #-}

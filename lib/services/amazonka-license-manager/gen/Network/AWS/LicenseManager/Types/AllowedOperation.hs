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
-- Module      : Network.AWS.LicenseManager.Types.AllowedOperation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LicenseManager.Types.AllowedOperation
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AllowedOperation = AllowedOperation'
  { fromAllowedOperation ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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

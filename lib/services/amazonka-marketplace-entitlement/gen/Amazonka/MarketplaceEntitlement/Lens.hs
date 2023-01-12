{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MarketplaceEntitlement.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MarketplaceEntitlement.Lens
  ( -- * Operations

    -- ** GetEntitlements
    getEntitlements_filter,
    getEntitlements_maxResults,
    getEntitlements_nextToken,
    getEntitlements_productCode,
    getEntitlementsResponse_entitlements,
    getEntitlementsResponse_nextToken,
    getEntitlementsResponse_httpStatus,

    -- * Types

    -- ** Entitlement
    entitlement_customerIdentifier,
    entitlement_dimension,
    entitlement_expirationDate,
    entitlement_productCode,
    entitlement_value,

    -- ** EntitlementValue
    entitlementValue_booleanValue,
    entitlementValue_doubleValue,
    entitlementValue_integerValue,
    entitlementValue_stringValue,
  )
where

import Amazonka.MarketplaceEntitlement.GetEntitlements
import Amazonka.MarketplaceEntitlement.Types.Entitlement
import Amazonka.MarketplaceEntitlement.Types.EntitlementValue

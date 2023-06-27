{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Account.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Account.Lens
  ( -- * Operations

    -- ** DeleteAlternateContact
    deleteAlternateContact_accountId,
    deleteAlternateContact_alternateContactType,

    -- ** DisableRegion
    disableRegion_accountId,
    disableRegion_regionName,

    -- ** EnableRegion
    enableRegion_accountId,
    enableRegion_regionName,

    -- ** GetAlternateContact
    getAlternateContact_accountId,
    getAlternateContact_alternateContactType,
    getAlternateContactResponse_alternateContact,
    getAlternateContactResponse_httpStatus,

    -- ** GetContactInformation
    getContactInformation_accountId,
    getContactInformationResponse_contactInformation,
    getContactInformationResponse_httpStatus,

    -- ** GetRegionOptStatus
    getRegionOptStatus_accountId,
    getRegionOptStatus_regionName,
    getRegionOptStatusResponse_regionName,
    getRegionOptStatusResponse_regionOptStatus,
    getRegionOptStatusResponse_httpStatus,

    -- ** ListRegions
    listRegions_accountId,
    listRegions_maxResults,
    listRegions_nextToken,
    listRegions_regionOptStatusContains,
    listRegionsResponse_nextToken,
    listRegionsResponse_regions,
    listRegionsResponse_httpStatus,

    -- ** PutAlternateContact
    putAlternateContact_accountId,
    putAlternateContact_alternateContactType,
    putAlternateContact_emailAddress,
    putAlternateContact_name,
    putAlternateContact_phoneNumber,
    putAlternateContact_title,

    -- ** PutContactInformation
    putContactInformation_accountId,
    putContactInformation_contactInformation,

    -- * Types

    -- ** AlternateContact
    alternateContact_alternateContactType,
    alternateContact_emailAddress,
    alternateContact_name,
    alternateContact_phoneNumber,
    alternateContact_title,

    -- ** ContactInformation
    contactInformation_addressLine2,
    contactInformation_addressLine3,
    contactInformation_companyName,
    contactInformation_districtOrCounty,
    contactInformation_stateOrRegion,
    contactInformation_websiteUrl,
    contactInformation_addressLine1,
    contactInformation_city,
    contactInformation_countryCode,
    contactInformation_fullName,
    contactInformation_phoneNumber,
    contactInformation_postalCode,

    -- ** Region
    region_regionName,
    region_regionOptStatus,
  )
where

import Amazonka.Account.DeleteAlternateContact
import Amazonka.Account.DisableRegion
import Amazonka.Account.EnableRegion
import Amazonka.Account.GetAlternateContact
import Amazonka.Account.GetContactInformation
import Amazonka.Account.GetRegionOptStatus
import Amazonka.Account.ListRegions
import Amazonka.Account.PutAlternateContact
import Amazonka.Account.PutContactInformation
import Amazonka.Account.Types.AlternateContact
import Amazonka.Account.Types.ContactInformation
import Amazonka.Account.Types.Region

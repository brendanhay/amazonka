{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Route53.GetGeoLocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about whether a specified geographic location is
-- supported for Amazon Route 53 geolocation resource record sets.
--
-- Route 53 does not perform authorization for this API because it
-- retrieves information that is already available to the public.
--
-- Use the following syntax to determine whether a continent is supported
-- for geolocation:
--
-- @GET \/2013-04-01\/geolocation?continentcode=two-letter abbreviation for a continent @
--
-- Use the following syntax to determine whether a country is supported for
-- geolocation:
--
-- @GET \/2013-04-01\/geolocation?countrycode=two-character country code @
--
-- Use the following syntax to determine whether a subdivision of a country
-- is supported for geolocation:
--
-- @GET \/2013-04-01\/geolocation?countrycode=two-character country code&subdivisioncode=subdivision code @
module Amazonka.Route53.GetGeoLocation
  ( -- * Creating a Request
    GetGeoLocation (..),
    newGetGeoLocation,

    -- * Request Lenses
    getGeoLocation_continentCode,
    getGeoLocation_countryCode,
    getGeoLocation_subdivisionCode,

    -- * Destructuring the Response
    GetGeoLocationResponse (..),
    newGetGeoLocationResponse,

    -- * Response Lenses
    getGeoLocationResponse_httpStatus,
    getGeoLocationResponse_geoLocationDetails,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A request for information about whether a specified geographic location
-- is supported for Amazon Route 53 geolocation resource record sets.
--
-- /See:/ 'newGetGeoLocation' smart constructor.
data GetGeoLocation = GetGeoLocation'
  { -- | For geolocation resource record sets, a two-letter abbreviation that
    -- identifies a continent. Amazon Route 53 supports the following continent
    -- codes:
    --
    -- -   __AF__: Africa
    --
    -- -   __AN__: Antarctica
    --
    -- -   __AS__: Asia
    --
    -- -   __EU__: Europe
    --
    -- -   __OC__: Oceania
    --
    -- -   __NA__: North America
    --
    -- -   __SA__: South America
    continentCode :: Prelude.Maybe Prelude.Text,
    -- | Amazon Route 53 uses the two-letter country codes that are specified in
    -- <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO standard 3166-1 alpha-2>.
    countryCode :: Prelude.Maybe Prelude.Text,
    -- | The code for the subdivision, such as a particular state within the
    -- United States. For a list of US state abbreviations, see
    -- <https://pe.usps.com/text/pub28/28apb.htm Appendix B: Two–Letter State and Possession Abbreviations>
    -- on the United States Postal Service website. For a list of all supported
    -- subdivision codes, use the
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListGeoLocations.html ListGeoLocations>
    -- API.
    subdivisionCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGeoLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'continentCode', 'getGeoLocation_continentCode' - For geolocation resource record sets, a two-letter abbreviation that
-- identifies a continent. Amazon Route 53 supports the following continent
-- codes:
--
-- -   __AF__: Africa
--
-- -   __AN__: Antarctica
--
-- -   __AS__: Asia
--
-- -   __EU__: Europe
--
-- -   __OC__: Oceania
--
-- -   __NA__: North America
--
-- -   __SA__: South America
--
-- 'countryCode', 'getGeoLocation_countryCode' - Amazon Route 53 uses the two-letter country codes that are specified in
-- <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO standard 3166-1 alpha-2>.
--
-- 'subdivisionCode', 'getGeoLocation_subdivisionCode' - The code for the subdivision, such as a particular state within the
-- United States. For a list of US state abbreviations, see
-- <https://pe.usps.com/text/pub28/28apb.htm Appendix B: Two–Letter State and Possession Abbreviations>
-- on the United States Postal Service website. For a list of all supported
-- subdivision codes, use the
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListGeoLocations.html ListGeoLocations>
-- API.
newGetGeoLocation ::
  GetGeoLocation
newGetGeoLocation =
  GetGeoLocation'
    { continentCode = Prelude.Nothing,
      countryCode = Prelude.Nothing,
      subdivisionCode = Prelude.Nothing
    }

-- | For geolocation resource record sets, a two-letter abbreviation that
-- identifies a continent. Amazon Route 53 supports the following continent
-- codes:
--
-- -   __AF__: Africa
--
-- -   __AN__: Antarctica
--
-- -   __AS__: Asia
--
-- -   __EU__: Europe
--
-- -   __OC__: Oceania
--
-- -   __NA__: North America
--
-- -   __SA__: South America
getGeoLocation_continentCode :: Lens.Lens' GetGeoLocation (Prelude.Maybe Prelude.Text)
getGeoLocation_continentCode = Lens.lens (\GetGeoLocation' {continentCode} -> continentCode) (\s@GetGeoLocation' {} a -> s {continentCode = a} :: GetGeoLocation)

-- | Amazon Route 53 uses the two-letter country codes that are specified in
-- <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO standard 3166-1 alpha-2>.
getGeoLocation_countryCode :: Lens.Lens' GetGeoLocation (Prelude.Maybe Prelude.Text)
getGeoLocation_countryCode = Lens.lens (\GetGeoLocation' {countryCode} -> countryCode) (\s@GetGeoLocation' {} a -> s {countryCode = a} :: GetGeoLocation)

-- | The code for the subdivision, such as a particular state within the
-- United States. For a list of US state abbreviations, see
-- <https://pe.usps.com/text/pub28/28apb.htm Appendix B: Two–Letter State and Possession Abbreviations>
-- on the United States Postal Service website. For a list of all supported
-- subdivision codes, use the
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListGeoLocations.html ListGeoLocations>
-- API.
getGeoLocation_subdivisionCode :: Lens.Lens' GetGeoLocation (Prelude.Maybe Prelude.Text)
getGeoLocation_subdivisionCode = Lens.lens (\GetGeoLocation' {subdivisionCode} -> subdivisionCode) (\s@GetGeoLocation' {} a -> s {subdivisionCode = a} :: GetGeoLocation)

instance Core.AWSRequest GetGeoLocation where
  type
    AWSResponse GetGeoLocation =
      GetGeoLocationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetGeoLocationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "GeoLocationDetails")
      )

instance Prelude.Hashable GetGeoLocation where
  hashWithSalt _salt GetGeoLocation' {..} =
    _salt `Prelude.hashWithSalt` continentCode
      `Prelude.hashWithSalt` countryCode
      `Prelude.hashWithSalt` subdivisionCode

instance Prelude.NFData GetGeoLocation where
  rnf GetGeoLocation' {..} =
    Prelude.rnf continentCode
      `Prelude.seq` Prelude.rnf countryCode
      `Prelude.seq` Prelude.rnf subdivisionCode

instance Data.ToHeaders GetGeoLocation where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetGeoLocation where
  toPath = Prelude.const "/2013-04-01/geolocation"

instance Data.ToQuery GetGeoLocation where
  toQuery GetGeoLocation' {..} =
    Prelude.mconcat
      [ "continentcode" Data.=: continentCode,
        "countrycode" Data.=: countryCode,
        "subdivisioncode" Data.=: subdivisionCode
      ]

-- | A complex type that contains the response information for the specified
-- geolocation code.
--
-- /See:/ 'newGetGeoLocationResponse' smart constructor.
data GetGeoLocationResponse = GetGeoLocationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that contains the codes and full continent, country, and
    -- subdivision names for the specified geolocation code.
    geoLocationDetails :: GeoLocationDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGeoLocationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getGeoLocationResponse_httpStatus' - The response's http status code.
--
-- 'geoLocationDetails', 'getGeoLocationResponse_geoLocationDetails' - A complex type that contains the codes and full continent, country, and
-- subdivision names for the specified geolocation code.
newGetGeoLocationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'geoLocationDetails'
  GeoLocationDetails ->
  GetGeoLocationResponse
newGetGeoLocationResponse
  pHttpStatus_
  pGeoLocationDetails_ =
    GetGeoLocationResponse'
      { httpStatus = pHttpStatus_,
        geoLocationDetails = pGeoLocationDetails_
      }

-- | The response's http status code.
getGeoLocationResponse_httpStatus :: Lens.Lens' GetGeoLocationResponse Prelude.Int
getGeoLocationResponse_httpStatus = Lens.lens (\GetGeoLocationResponse' {httpStatus} -> httpStatus) (\s@GetGeoLocationResponse' {} a -> s {httpStatus = a} :: GetGeoLocationResponse)

-- | A complex type that contains the codes and full continent, country, and
-- subdivision names for the specified geolocation code.
getGeoLocationResponse_geoLocationDetails :: Lens.Lens' GetGeoLocationResponse GeoLocationDetails
getGeoLocationResponse_geoLocationDetails = Lens.lens (\GetGeoLocationResponse' {geoLocationDetails} -> geoLocationDetails) (\s@GetGeoLocationResponse' {} a -> s {geoLocationDetails = a} :: GetGeoLocationResponse)

instance Prelude.NFData GetGeoLocationResponse where
  rnf GetGeoLocationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf geoLocationDetails

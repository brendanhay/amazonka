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
-- Module      : Amazonka.Route53.ListGeoLocations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of supported geographic locations.
--
-- Countries are listed first, and continents are listed last. If Amazon
-- Route 53 supports subdivisions for a country (for example, states or
-- provinces), the subdivisions for that country are listed in alphabetical
-- order immediately after the corresponding country.
--
-- Route 53 does not perform authorization for this API because it
-- retrieves information that is already available to the public.
--
-- For a list of supported geolocation codes, see the
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GeoLocation.html GeoLocation>
-- data type.
module Amazonka.Route53.ListGeoLocations
  ( -- * Creating a Request
    ListGeoLocations (..),
    newListGeoLocations,

    -- * Request Lenses
    listGeoLocations_maxItems,
    listGeoLocations_startContinentCode,
    listGeoLocations_startCountryCode,
    listGeoLocations_startSubdivisionCode,

    -- * Destructuring the Response
    ListGeoLocationsResponse (..),
    newListGeoLocationsResponse,

    -- * Response Lenses
    listGeoLocationsResponse_nextContinentCode,
    listGeoLocationsResponse_nextCountryCode,
    listGeoLocationsResponse_nextSubdivisionCode,
    listGeoLocationsResponse_httpStatus,
    listGeoLocationsResponse_geoLocationDetailsList,
    listGeoLocationsResponse_isTruncated,
    listGeoLocationsResponse_maxItems,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A request to get a list of geographic locations that Amazon Route 53
-- supports for geolocation resource record sets.
--
-- /See:/ 'newListGeoLocations' smart constructor.
data ListGeoLocations = ListGeoLocations'
  { -- | (Optional) The maximum number of geolocations to be included in the
    -- response body for this request. If more than @maxitems@ geolocations
    -- remain to be listed, then the value of the @IsTruncated@ element in the
    -- response is @true@.
    maxItems :: Prelude.Maybe Prelude.Text,
    -- | The code for the continent with which you want to start listing
    -- locations that Amazon Route 53 supports for geolocation. If Route 53 has
    -- already returned a page or more of results, if @IsTruncated@ is true,
    -- and if @NextContinentCode@ from the previous response has a value, enter
    -- that value in @startcontinentcode@ to return the next page of results.
    --
    -- Include @startcontinentcode@ only if you want to list continents. Don\'t
    -- include @startcontinentcode@ when you\'re listing countries or countries
    -- with their subdivisions.
    startContinentCode :: Prelude.Maybe Prelude.Text,
    -- | The code for the country with which you want to start listing locations
    -- that Amazon Route 53 supports for geolocation. If Route 53 has already
    -- returned a page or more of results, if @IsTruncated@ is @true@, and if
    -- @NextCountryCode@ from the previous response has a value, enter that
    -- value in @startcountrycode@ to return the next page of results.
    startCountryCode :: Prelude.Maybe Prelude.Text,
    -- | The code for the state of the United States with which you want to start
    -- listing locations that Amazon Route 53 supports for geolocation. If
    -- Route 53 has already returned a page or more of results, if
    -- @IsTruncated@ is @true@, and if @NextSubdivisionCode@ from the previous
    -- response has a value, enter that value in @startsubdivisioncode@ to
    -- return the next page of results.
    --
    -- To list subdivisions (U.S. states), you must include both
    -- @startcountrycode@ and @startsubdivisioncode@.
    startSubdivisionCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGeoLocations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listGeoLocations_maxItems' - (Optional) The maximum number of geolocations to be included in the
-- response body for this request. If more than @maxitems@ geolocations
-- remain to be listed, then the value of the @IsTruncated@ element in the
-- response is @true@.
--
-- 'startContinentCode', 'listGeoLocations_startContinentCode' - The code for the continent with which you want to start listing
-- locations that Amazon Route 53 supports for geolocation. If Route 53 has
-- already returned a page or more of results, if @IsTruncated@ is true,
-- and if @NextContinentCode@ from the previous response has a value, enter
-- that value in @startcontinentcode@ to return the next page of results.
--
-- Include @startcontinentcode@ only if you want to list continents. Don\'t
-- include @startcontinentcode@ when you\'re listing countries or countries
-- with their subdivisions.
--
-- 'startCountryCode', 'listGeoLocations_startCountryCode' - The code for the country with which you want to start listing locations
-- that Amazon Route 53 supports for geolocation. If Route 53 has already
-- returned a page or more of results, if @IsTruncated@ is @true@, and if
-- @NextCountryCode@ from the previous response has a value, enter that
-- value in @startcountrycode@ to return the next page of results.
--
-- 'startSubdivisionCode', 'listGeoLocations_startSubdivisionCode' - The code for the state of the United States with which you want to start
-- listing locations that Amazon Route 53 supports for geolocation. If
-- Route 53 has already returned a page or more of results, if
-- @IsTruncated@ is @true@, and if @NextSubdivisionCode@ from the previous
-- response has a value, enter that value in @startsubdivisioncode@ to
-- return the next page of results.
--
-- To list subdivisions (U.S. states), you must include both
-- @startcountrycode@ and @startsubdivisioncode@.
newListGeoLocations ::
  ListGeoLocations
newListGeoLocations =
  ListGeoLocations'
    { maxItems = Prelude.Nothing,
      startContinentCode = Prelude.Nothing,
      startCountryCode = Prelude.Nothing,
      startSubdivisionCode = Prelude.Nothing
    }

-- | (Optional) The maximum number of geolocations to be included in the
-- response body for this request. If more than @maxitems@ geolocations
-- remain to be listed, then the value of the @IsTruncated@ element in the
-- response is @true@.
listGeoLocations_maxItems :: Lens.Lens' ListGeoLocations (Prelude.Maybe Prelude.Text)
listGeoLocations_maxItems = Lens.lens (\ListGeoLocations' {maxItems} -> maxItems) (\s@ListGeoLocations' {} a -> s {maxItems = a} :: ListGeoLocations)

-- | The code for the continent with which you want to start listing
-- locations that Amazon Route 53 supports for geolocation. If Route 53 has
-- already returned a page or more of results, if @IsTruncated@ is true,
-- and if @NextContinentCode@ from the previous response has a value, enter
-- that value in @startcontinentcode@ to return the next page of results.
--
-- Include @startcontinentcode@ only if you want to list continents. Don\'t
-- include @startcontinentcode@ when you\'re listing countries or countries
-- with their subdivisions.
listGeoLocations_startContinentCode :: Lens.Lens' ListGeoLocations (Prelude.Maybe Prelude.Text)
listGeoLocations_startContinentCode = Lens.lens (\ListGeoLocations' {startContinentCode} -> startContinentCode) (\s@ListGeoLocations' {} a -> s {startContinentCode = a} :: ListGeoLocations)

-- | The code for the country with which you want to start listing locations
-- that Amazon Route 53 supports for geolocation. If Route 53 has already
-- returned a page or more of results, if @IsTruncated@ is @true@, and if
-- @NextCountryCode@ from the previous response has a value, enter that
-- value in @startcountrycode@ to return the next page of results.
listGeoLocations_startCountryCode :: Lens.Lens' ListGeoLocations (Prelude.Maybe Prelude.Text)
listGeoLocations_startCountryCode = Lens.lens (\ListGeoLocations' {startCountryCode} -> startCountryCode) (\s@ListGeoLocations' {} a -> s {startCountryCode = a} :: ListGeoLocations)

-- | The code for the state of the United States with which you want to start
-- listing locations that Amazon Route 53 supports for geolocation. If
-- Route 53 has already returned a page or more of results, if
-- @IsTruncated@ is @true@, and if @NextSubdivisionCode@ from the previous
-- response has a value, enter that value in @startsubdivisioncode@ to
-- return the next page of results.
--
-- To list subdivisions (U.S. states), you must include both
-- @startcountrycode@ and @startsubdivisioncode@.
listGeoLocations_startSubdivisionCode :: Lens.Lens' ListGeoLocations (Prelude.Maybe Prelude.Text)
listGeoLocations_startSubdivisionCode = Lens.lens (\ListGeoLocations' {startSubdivisionCode} -> startSubdivisionCode) (\s@ListGeoLocations' {} a -> s {startSubdivisionCode = a} :: ListGeoLocations)

instance Core.AWSRequest ListGeoLocations where
  type
    AWSResponse ListGeoLocations =
      ListGeoLocationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListGeoLocationsResponse'
            Prelude.<$> (x Data..@? "NextContinentCode")
            Prelude.<*> (x Data..@? "NextCountryCode")
            Prelude.<*> (x Data..@? "NextSubdivisionCode")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..@? "GeoLocationDetailsList"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "GeoLocationDetails"
                        )
            Prelude.<*> (x Data..@ "IsTruncated")
            Prelude.<*> (x Data..@ "MaxItems")
      )

instance Prelude.Hashable ListGeoLocations where
  hashWithSalt _salt ListGeoLocations' {..} =
    _salt
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` startContinentCode
      `Prelude.hashWithSalt` startCountryCode
      `Prelude.hashWithSalt` startSubdivisionCode

instance Prelude.NFData ListGeoLocations where
  rnf ListGeoLocations' {..} =
    Prelude.rnf maxItems `Prelude.seq`
      Prelude.rnf startContinentCode `Prelude.seq`
        Prelude.rnf startCountryCode `Prelude.seq`
          Prelude.rnf startSubdivisionCode

instance Data.ToHeaders ListGeoLocations where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListGeoLocations where
  toPath = Prelude.const "/2013-04-01/geolocations"

instance Data.ToQuery ListGeoLocations where
  toQuery ListGeoLocations' {..} =
    Prelude.mconcat
      [ "maxitems" Data.=: maxItems,
        "startcontinentcode" Data.=: startContinentCode,
        "startcountrycode" Data.=: startCountryCode,
        "startsubdivisioncode" Data.=: startSubdivisionCode
      ]

-- | A complex type containing the response information for the request.
--
-- /See:/ 'newListGeoLocationsResponse' smart constructor.
data ListGeoLocationsResponse = ListGeoLocationsResponse'
  { -- | If @IsTruncated@ is @true@, you can make a follow-up request to display
    -- more locations. Enter the value of @NextContinentCode@ in the
    -- @startcontinentcode@ parameter in another @ListGeoLocations@ request.
    nextContinentCode :: Prelude.Maybe Prelude.Text,
    -- | If @IsTruncated@ is @true@, you can make a follow-up request to display
    -- more locations. Enter the value of @NextCountryCode@ in the
    -- @startcountrycode@ parameter in another @ListGeoLocations@ request.
    nextCountryCode :: Prelude.Maybe Prelude.Text,
    -- | If @IsTruncated@ is @true@, you can make a follow-up request to display
    -- more locations. Enter the value of @NextSubdivisionCode@ in the
    -- @startsubdivisioncode@ parameter in another @ListGeoLocations@ request.
    nextSubdivisionCode :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that contains one @GeoLocationDetails@ element for each
    -- location that Amazon Route 53 supports for geolocation.
    geoLocationDetailsList :: [GeoLocationDetails],
    -- | A value that indicates whether more locations remain to be listed after
    -- the last location in this response. If so, the value of @IsTruncated@ is
    -- @true@. To get more values, submit another request and include the
    -- values of @NextContinentCode@, @NextCountryCode@, and
    -- @NextSubdivisionCode@ in the @startcontinentcode@, @startcountrycode@,
    -- and @startsubdivisioncode@, as applicable.
    isTruncated :: Prelude.Bool,
    -- | The value that you specified for @MaxItems@ in the request.
    maxItems :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGeoLocationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextContinentCode', 'listGeoLocationsResponse_nextContinentCode' - If @IsTruncated@ is @true@, you can make a follow-up request to display
-- more locations. Enter the value of @NextContinentCode@ in the
-- @startcontinentcode@ parameter in another @ListGeoLocations@ request.
--
-- 'nextCountryCode', 'listGeoLocationsResponse_nextCountryCode' - If @IsTruncated@ is @true@, you can make a follow-up request to display
-- more locations. Enter the value of @NextCountryCode@ in the
-- @startcountrycode@ parameter in another @ListGeoLocations@ request.
--
-- 'nextSubdivisionCode', 'listGeoLocationsResponse_nextSubdivisionCode' - If @IsTruncated@ is @true@, you can make a follow-up request to display
-- more locations. Enter the value of @NextSubdivisionCode@ in the
-- @startsubdivisioncode@ parameter in another @ListGeoLocations@ request.
--
-- 'httpStatus', 'listGeoLocationsResponse_httpStatus' - The response's http status code.
--
-- 'geoLocationDetailsList', 'listGeoLocationsResponse_geoLocationDetailsList' - A complex type that contains one @GeoLocationDetails@ element for each
-- location that Amazon Route 53 supports for geolocation.
--
-- 'isTruncated', 'listGeoLocationsResponse_isTruncated' - A value that indicates whether more locations remain to be listed after
-- the last location in this response. If so, the value of @IsTruncated@ is
-- @true@. To get more values, submit another request and include the
-- values of @NextContinentCode@, @NextCountryCode@, and
-- @NextSubdivisionCode@ in the @startcontinentcode@, @startcountrycode@,
-- and @startsubdivisioncode@, as applicable.
--
-- 'maxItems', 'listGeoLocationsResponse_maxItems' - The value that you specified for @MaxItems@ in the request.
newListGeoLocationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'isTruncated'
  Prelude.Bool ->
  -- | 'maxItems'
  Prelude.Text ->
  ListGeoLocationsResponse
newListGeoLocationsResponse
  pHttpStatus_
  pIsTruncated_
  pMaxItems_ =
    ListGeoLocationsResponse'
      { nextContinentCode =
          Prelude.Nothing,
        nextCountryCode = Prelude.Nothing,
        nextSubdivisionCode = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        geoLocationDetailsList = Prelude.mempty,
        isTruncated = pIsTruncated_,
        maxItems = pMaxItems_
      }

-- | If @IsTruncated@ is @true@, you can make a follow-up request to display
-- more locations. Enter the value of @NextContinentCode@ in the
-- @startcontinentcode@ parameter in another @ListGeoLocations@ request.
listGeoLocationsResponse_nextContinentCode :: Lens.Lens' ListGeoLocationsResponse (Prelude.Maybe Prelude.Text)
listGeoLocationsResponse_nextContinentCode = Lens.lens (\ListGeoLocationsResponse' {nextContinentCode} -> nextContinentCode) (\s@ListGeoLocationsResponse' {} a -> s {nextContinentCode = a} :: ListGeoLocationsResponse)

-- | If @IsTruncated@ is @true@, you can make a follow-up request to display
-- more locations. Enter the value of @NextCountryCode@ in the
-- @startcountrycode@ parameter in another @ListGeoLocations@ request.
listGeoLocationsResponse_nextCountryCode :: Lens.Lens' ListGeoLocationsResponse (Prelude.Maybe Prelude.Text)
listGeoLocationsResponse_nextCountryCode = Lens.lens (\ListGeoLocationsResponse' {nextCountryCode} -> nextCountryCode) (\s@ListGeoLocationsResponse' {} a -> s {nextCountryCode = a} :: ListGeoLocationsResponse)

-- | If @IsTruncated@ is @true@, you can make a follow-up request to display
-- more locations. Enter the value of @NextSubdivisionCode@ in the
-- @startsubdivisioncode@ parameter in another @ListGeoLocations@ request.
listGeoLocationsResponse_nextSubdivisionCode :: Lens.Lens' ListGeoLocationsResponse (Prelude.Maybe Prelude.Text)
listGeoLocationsResponse_nextSubdivisionCode = Lens.lens (\ListGeoLocationsResponse' {nextSubdivisionCode} -> nextSubdivisionCode) (\s@ListGeoLocationsResponse' {} a -> s {nextSubdivisionCode = a} :: ListGeoLocationsResponse)

-- | The response's http status code.
listGeoLocationsResponse_httpStatus :: Lens.Lens' ListGeoLocationsResponse Prelude.Int
listGeoLocationsResponse_httpStatus = Lens.lens (\ListGeoLocationsResponse' {httpStatus} -> httpStatus) (\s@ListGeoLocationsResponse' {} a -> s {httpStatus = a} :: ListGeoLocationsResponse)

-- | A complex type that contains one @GeoLocationDetails@ element for each
-- location that Amazon Route 53 supports for geolocation.
listGeoLocationsResponse_geoLocationDetailsList :: Lens.Lens' ListGeoLocationsResponse [GeoLocationDetails]
listGeoLocationsResponse_geoLocationDetailsList = Lens.lens (\ListGeoLocationsResponse' {geoLocationDetailsList} -> geoLocationDetailsList) (\s@ListGeoLocationsResponse' {} a -> s {geoLocationDetailsList = a} :: ListGeoLocationsResponse) Prelude.. Lens.coerced

-- | A value that indicates whether more locations remain to be listed after
-- the last location in this response. If so, the value of @IsTruncated@ is
-- @true@. To get more values, submit another request and include the
-- values of @NextContinentCode@, @NextCountryCode@, and
-- @NextSubdivisionCode@ in the @startcontinentcode@, @startcountrycode@,
-- and @startsubdivisioncode@, as applicable.
listGeoLocationsResponse_isTruncated :: Lens.Lens' ListGeoLocationsResponse Prelude.Bool
listGeoLocationsResponse_isTruncated = Lens.lens (\ListGeoLocationsResponse' {isTruncated} -> isTruncated) (\s@ListGeoLocationsResponse' {} a -> s {isTruncated = a} :: ListGeoLocationsResponse)

-- | The value that you specified for @MaxItems@ in the request.
listGeoLocationsResponse_maxItems :: Lens.Lens' ListGeoLocationsResponse Prelude.Text
listGeoLocationsResponse_maxItems = Lens.lens (\ListGeoLocationsResponse' {maxItems} -> maxItems) (\s@ListGeoLocationsResponse' {} a -> s {maxItems = a} :: ListGeoLocationsResponse)

instance Prelude.NFData ListGeoLocationsResponse where
  rnf ListGeoLocationsResponse' {..} =
    Prelude.rnf nextContinentCode `Prelude.seq`
      Prelude.rnf nextCountryCode `Prelude.seq`
        Prelude.rnf nextSubdivisionCode `Prelude.seq`
          Prelude.rnf httpStatus `Prelude.seq`
            Prelude.rnf geoLocationDetailsList `Prelude.seq`
              Prelude.rnf isTruncated `Prelude.seq`
                Prelude.rnf maxItems

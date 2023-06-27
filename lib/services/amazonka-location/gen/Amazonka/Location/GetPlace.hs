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
-- Module      : Amazonka.Location.GetPlace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Finds a place by its unique ID. A @PlaceId@ is returned by other search
-- operations.
--
-- A PlaceId is valid only if all of the following are the same in the
-- original search request and the call to @GetPlace@.
--
-- -   Customer Amazon Web Services account
--
-- -   Amazon Web Services Region
--
-- -   Data provider specified in the place index resource
module Amazonka.Location.GetPlace
  ( -- * Creating a Request
    GetPlace (..),
    newGetPlace,

    -- * Request Lenses
    getPlace_language,
    getPlace_indexName,
    getPlace_placeId,

    -- * Destructuring the Response
    GetPlaceResponse (..),
    newGetPlaceResponse,

    -- * Response Lenses
    getPlaceResponse_httpStatus,
    getPlaceResponse_place,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPlace' smart constructor.
data GetPlace = GetPlace'
  { -- | The preferred language used to return results. The value must be a valid
    -- <https://tools.ietf.org/search/bcp47 BCP 47> language tag, for example,
    -- @en@ for English.
    --
    -- This setting affects the languages used in the results, but not the
    -- results themselves. If no language is specified, or not supported for a
    -- particular result, the partner automatically chooses a language for the
    -- result.
    --
    -- For an example, we\'ll use the Greek language. You search for a location
    -- around Athens, Greece, with the @language@ parameter set to @en@. The
    -- @city@ in the results will most likely be returned as @Athens@.
    --
    -- If you set the @language@ parameter to @el@, for Greek, then the @city@
    -- in the results will more likely be returned as @Αθήνα@.
    --
    -- If the data provider does not have a value for Greek, the result will be
    -- in a language that the provider does support.
    language :: Prelude.Maybe Prelude.Text,
    -- | The name of the place index resource that you want to use for the
    -- search.
    indexName :: Prelude.Text,
    -- | The identifier of the place to find.
    placeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPlace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'language', 'getPlace_language' - The preferred language used to return results. The value must be a valid
-- <https://tools.ietf.org/search/bcp47 BCP 47> language tag, for example,
-- @en@ for English.
--
-- This setting affects the languages used in the results, but not the
-- results themselves. If no language is specified, or not supported for a
-- particular result, the partner automatically chooses a language for the
-- result.
--
-- For an example, we\'ll use the Greek language. You search for a location
-- around Athens, Greece, with the @language@ parameter set to @en@. The
-- @city@ in the results will most likely be returned as @Athens@.
--
-- If you set the @language@ parameter to @el@, for Greek, then the @city@
-- in the results will more likely be returned as @Αθήνα@.
--
-- If the data provider does not have a value for Greek, the result will be
-- in a language that the provider does support.
--
-- 'indexName', 'getPlace_indexName' - The name of the place index resource that you want to use for the
-- search.
--
-- 'placeId', 'getPlace_placeId' - The identifier of the place to find.
newGetPlace ::
  -- | 'indexName'
  Prelude.Text ->
  -- | 'placeId'
  Prelude.Text ->
  GetPlace
newGetPlace pIndexName_ pPlaceId_ =
  GetPlace'
    { language = Prelude.Nothing,
      indexName = pIndexName_,
      placeId = pPlaceId_
    }

-- | The preferred language used to return results. The value must be a valid
-- <https://tools.ietf.org/search/bcp47 BCP 47> language tag, for example,
-- @en@ for English.
--
-- This setting affects the languages used in the results, but not the
-- results themselves. If no language is specified, or not supported for a
-- particular result, the partner automatically chooses a language for the
-- result.
--
-- For an example, we\'ll use the Greek language. You search for a location
-- around Athens, Greece, with the @language@ parameter set to @en@. The
-- @city@ in the results will most likely be returned as @Athens@.
--
-- If you set the @language@ parameter to @el@, for Greek, then the @city@
-- in the results will more likely be returned as @Αθήνα@.
--
-- If the data provider does not have a value for Greek, the result will be
-- in a language that the provider does support.
getPlace_language :: Lens.Lens' GetPlace (Prelude.Maybe Prelude.Text)
getPlace_language = Lens.lens (\GetPlace' {language} -> language) (\s@GetPlace' {} a -> s {language = a} :: GetPlace)

-- | The name of the place index resource that you want to use for the
-- search.
getPlace_indexName :: Lens.Lens' GetPlace Prelude.Text
getPlace_indexName = Lens.lens (\GetPlace' {indexName} -> indexName) (\s@GetPlace' {} a -> s {indexName = a} :: GetPlace)

-- | The identifier of the place to find.
getPlace_placeId :: Lens.Lens' GetPlace Prelude.Text
getPlace_placeId = Lens.lens (\GetPlace' {placeId} -> placeId) (\s@GetPlace' {} a -> s {placeId = a} :: GetPlace)

instance Core.AWSRequest GetPlace where
  type AWSResponse GetPlace = GetPlaceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPlaceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Place")
      )

instance Prelude.Hashable GetPlace where
  hashWithSalt _salt GetPlace' {..} =
    _salt
      `Prelude.hashWithSalt` language
      `Prelude.hashWithSalt` indexName
      `Prelude.hashWithSalt` placeId

instance Prelude.NFData GetPlace where
  rnf GetPlace' {..} =
    Prelude.rnf language
      `Prelude.seq` Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf placeId

instance Data.ToHeaders GetPlace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetPlace where
  toPath GetPlace' {..} =
    Prelude.mconcat
      [ "/places/v0/indexes/",
        Data.toBS indexName,
        "/places/",
        Data.toBS placeId
      ]

instance Data.ToQuery GetPlace where
  toQuery GetPlace' {..} =
    Prelude.mconcat ["language" Data.=: language]

-- | /See:/ 'newGetPlaceResponse' smart constructor.
data GetPlaceResponse = GetPlaceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Details about the result, such as its address and position.
    place :: Place
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPlaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getPlaceResponse_httpStatus' - The response's http status code.
--
-- 'place', 'getPlaceResponse_place' - Details about the result, such as its address and position.
newGetPlaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'place'
  Place ->
  GetPlaceResponse
newGetPlaceResponse pHttpStatus_ pPlace_ =
  GetPlaceResponse'
    { httpStatus = pHttpStatus_,
      place = pPlace_
    }

-- | The response's http status code.
getPlaceResponse_httpStatus :: Lens.Lens' GetPlaceResponse Prelude.Int
getPlaceResponse_httpStatus = Lens.lens (\GetPlaceResponse' {httpStatus} -> httpStatus) (\s@GetPlaceResponse' {} a -> s {httpStatus = a} :: GetPlaceResponse)

-- | Details about the result, such as its address and position.
getPlaceResponse_place :: Lens.Lens' GetPlaceResponse Place
getPlaceResponse_place = Lens.lens (\GetPlaceResponse' {place} -> place) (\s@GetPlaceResponse' {} a -> s {place = a} :: GetPlaceResponse)

instance Prelude.NFData GetPlaceResponse where
  rnf GetPlaceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf place

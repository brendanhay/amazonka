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
-- Module      : Amazonka.EMR.ListStudioSessionMappings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all user or group session mappings for the Amazon EMR
-- Studio specified by @StudioId@.
--
-- This operation returns paginated results.
module Amazonka.EMR.ListStudioSessionMappings
  ( -- * Creating a Request
    ListStudioSessionMappings (..),
    newListStudioSessionMappings,

    -- * Request Lenses
    listStudioSessionMappings_identityType,
    listStudioSessionMappings_marker,
    listStudioSessionMappings_studioId,

    -- * Destructuring the Response
    ListStudioSessionMappingsResponse (..),
    newListStudioSessionMappingsResponse,

    -- * Response Lenses
    listStudioSessionMappingsResponse_marker,
    listStudioSessionMappingsResponse_sessionMappings,
    listStudioSessionMappingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListStudioSessionMappings' smart constructor.
data ListStudioSessionMappings = ListStudioSessionMappings'
  { -- | Specifies whether to return session mappings for users or groups. If not
    -- specified, the results include session mapping details for both users
    -- and groups.
    identityType :: Prelude.Maybe IdentityType,
    -- | The pagination token that indicates the set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon EMR Studio.
    studioId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStudioSessionMappings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityType', 'listStudioSessionMappings_identityType' - Specifies whether to return session mappings for users or groups. If not
-- specified, the results include session mapping details for both users
-- and groups.
--
-- 'marker', 'listStudioSessionMappings_marker' - The pagination token that indicates the set of results to retrieve.
--
-- 'studioId', 'listStudioSessionMappings_studioId' - The ID of the Amazon EMR Studio.
newListStudioSessionMappings ::
  ListStudioSessionMappings
newListStudioSessionMappings =
  ListStudioSessionMappings'
    { identityType =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      studioId = Prelude.Nothing
    }

-- | Specifies whether to return session mappings for users or groups. If not
-- specified, the results include session mapping details for both users
-- and groups.
listStudioSessionMappings_identityType :: Lens.Lens' ListStudioSessionMappings (Prelude.Maybe IdentityType)
listStudioSessionMappings_identityType = Lens.lens (\ListStudioSessionMappings' {identityType} -> identityType) (\s@ListStudioSessionMappings' {} a -> s {identityType = a} :: ListStudioSessionMappings)

-- | The pagination token that indicates the set of results to retrieve.
listStudioSessionMappings_marker :: Lens.Lens' ListStudioSessionMappings (Prelude.Maybe Prelude.Text)
listStudioSessionMappings_marker = Lens.lens (\ListStudioSessionMappings' {marker} -> marker) (\s@ListStudioSessionMappings' {} a -> s {marker = a} :: ListStudioSessionMappings)

-- | The ID of the Amazon EMR Studio.
listStudioSessionMappings_studioId :: Lens.Lens' ListStudioSessionMappings (Prelude.Maybe Prelude.Text)
listStudioSessionMappings_studioId = Lens.lens (\ListStudioSessionMappings' {studioId} -> studioId) (\s@ListStudioSessionMappings' {} a -> s {studioId = a} :: ListStudioSessionMappings)

instance Core.AWSPager ListStudioSessionMappings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStudioSessionMappingsResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listStudioSessionMappingsResponse_sessionMappings
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listStudioSessionMappings_marker
              Lens..~ rs
              Lens.^? listStudioSessionMappingsResponse_marker
              Prelude.. Lens._Just

instance Core.AWSRequest ListStudioSessionMappings where
  type
    AWSResponse ListStudioSessionMappings =
      ListStudioSessionMappingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStudioSessionMappingsResponse'
            Prelude.<$> (x Data..?> "Marker")
            Prelude.<*> ( x
                            Data..?> "SessionMappings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStudioSessionMappings where
  hashWithSalt _salt ListStudioSessionMappings' {..} =
    _salt
      `Prelude.hashWithSalt` identityType
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData ListStudioSessionMappings where
  rnf ListStudioSessionMappings' {..} =
    Prelude.rnf identityType `Prelude.seq`
      Prelude.rnf marker `Prelude.seq`
        Prelude.rnf studioId

instance Data.ToHeaders ListStudioSessionMappings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.ListStudioSessionMappings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListStudioSessionMappings where
  toJSON ListStudioSessionMappings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IdentityType" Data..=) Prelude.<$> identityType,
            ("Marker" Data..=) Prelude.<$> marker,
            ("StudioId" Data..=) Prelude.<$> studioId
          ]
      )

instance Data.ToPath ListStudioSessionMappings where
  toPath = Prelude.const "/"

instance Data.ToQuery ListStudioSessionMappings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListStudioSessionMappingsResponse' smart constructor.
data ListStudioSessionMappingsResponse = ListStudioSessionMappingsResponse'
  { -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of session mapping summary objects. Each object includes session
    -- mapping details such as creation time, identity type (user or group),
    -- and Amazon EMR Studio ID.
    sessionMappings :: Prelude.Maybe [SessionMappingSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStudioSessionMappingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listStudioSessionMappingsResponse_marker' - The pagination token that indicates the next set of results to retrieve.
--
-- 'sessionMappings', 'listStudioSessionMappingsResponse_sessionMappings' - A list of session mapping summary objects. Each object includes session
-- mapping details such as creation time, identity type (user or group),
-- and Amazon EMR Studio ID.
--
-- 'httpStatus', 'listStudioSessionMappingsResponse_httpStatus' - The response's http status code.
newListStudioSessionMappingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStudioSessionMappingsResponse
newListStudioSessionMappingsResponse pHttpStatus_ =
  ListStudioSessionMappingsResponse'
    { marker =
        Prelude.Nothing,
      sessionMappings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token that indicates the next set of results to retrieve.
listStudioSessionMappingsResponse_marker :: Lens.Lens' ListStudioSessionMappingsResponse (Prelude.Maybe Prelude.Text)
listStudioSessionMappingsResponse_marker = Lens.lens (\ListStudioSessionMappingsResponse' {marker} -> marker) (\s@ListStudioSessionMappingsResponse' {} a -> s {marker = a} :: ListStudioSessionMappingsResponse)

-- | A list of session mapping summary objects. Each object includes session
-- mapping details such as creation time, identity type (user or group),
-- and Amazon EMR Studio ID.
listStudioSessionMappingsResponse_sessionMappings :: Lens.Lens' ListStudioSessionMappingsResponse (Prelude.Maybe [SessionMappingSummary])
listStudioSessionMappingsResponse_sessionMappings = Lens.lens (\ListStudioSessionMappingsResponse' {sessionMappings} -> sessionMappings) (\s@ListStudioSessionMappingsResponse' {} a -> s {sessionMappings = a} :: ListStudioSessionMappingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listStudioSessionMappingsResponse_httpStatus :: Lens.Lens' ListStudioSessionMappingsResponse Prelude.Int
listStudioSessionMappingsResponse_httpStatus = Lens.lens (\ListStudioSessionMappingsResponse' {httpStatus} -> httpStatus) (\s@ListStudioSessionMappingsResponse' {} a -> s {httpStatus = a} :: ListStudioSessionMappingsResponse)

instance
  Prelude.NFData
    ListStudioSessionMappingsResponse
  where
  rnf ListStudioSessionMappingsResponse' {..} =
    Prelude.rnf marker `Prelude.seq`
      Prelude.rnf sessionMappings `Prelude.seq`
        Prelude.rnf httpStatus

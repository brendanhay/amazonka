{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EMR.ListStudioSessionMappings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all user or group session mappings for the Amazon EMR
-- Studio specified by @StudioId@.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListStudioSessionMappings
  ( -- * Creating a Request
    ListStudioSessionMappings (..),
    newListStudioSessionMappings,

    -- * Request Lenses
    listStudioSessionMappings_identityType,
    listStudioSessionMappings_studioId,
    listStudioSessionMappings_marker,

    -- * Destructuring the Response
    ListStudioSessionMappingsResponse (..),
    newListStudioSessionMappingsResponse,

    -- * Response Lenses
    listStudioSessionMappingsResponse_sessionMappings,
    listStudioSessionMappingsResponse_marker,
    listStudioSessionMappingsResponse_httpStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListStudioSessionMappings' smart constructor.
data ListStudioSessionMappings = ListStudioSessionMappings'
  { -- | Specifies whether to return session mappings for users or groups. If not
    -- specified, the results include session mapping details for both users
    -- and groups.
    identityType :: Prelude.Maybe IdentityType,
    -- | The ID of the Amazon EMR Studio.
    studioId :: Prelude.Maybe Prelude.Text,
    -- | The pagination token that indicates the set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'studioId', 'listStudioSessionMappings_studioId' - The ID of the Amazon EMR Studio.
--
-- 'marker', 'listStudioSessionMappings_marker' - The pagination token that indicates the set of results to retrieve.
newListStudioSessionMappings ::
  ListStudioSessionMappings
newListStudioSessionMappings =
  ListStudioSessionMappings'
    { identityType =
        Prelude.Nothing,
      studioId = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | Specifies whether to return session mappings for users or groups. If not
-- specified, the results include session mapping details for both users
-- and groups.
listStudioSessionMappings_identityType :: Lens.Lens' ListStudioSessionMappings (Prelude.Maybe IdentityType)
listStudioSessionMappings_identityType = Lens.lens (\ListStudioSessionMappings' {identityType} -> identityType) (\s@ListStudioSessionMappings' {} a -> s {identityType = a} :: ListStudioSessionMappings)

-- | The ID of the Amazon EMR Studio.
listStudioSessionMappings_studioId :: Lens.Lens' ListStudioSessionMappings (Prelude.Maybe Prelude.Text)
listStudioSessionMappings_studioId = Lens.lens (\ListStudioSessionMappings' {studioId} -> studioId) (\s@ListStudioSessionMappings' {} a -> s {studioId = a} :: ListStudioSessionMappings)

-- | The pagination token that indicates the set of results to retrieve.
listStudioSessionMappings_marker :: Lens.Lens' ListStudioSessionMappings (Prelude.Maybe Prelude.Text)
listStudioSessionMappings_marker = Lens.lens (\ListStudioSessionMappings' {marker} -> marker) (\s@ListStudioSessionMappings' {} a -> s {marker = a} :: ListStudioSessionMappings)

instance Pager.AWSPager ListStudioSessionMappings where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listStudioSessionMappingsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listStudioSessionMappingsResponse_sessionMappings
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listStudioSessionMappings_marker
          Lens..~ rs
          Lens.^? listStudioSessionMappingsResponse_marker
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListStudioSessionMappings where
  type
    Rs ListStudioSessionMappings =
      ListStudioSessionMappingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStudioSessionMappingsResponse'
            Prelude.<$> ( x Prelude..?> "SessionMappings"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStudioSessionMappings

instance Prelude.NFData ListStudioSessionMappings

instance Prelude.ToHeaders ListStudioSessionMappings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.ListStudioSessionMappings" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListStudioSessionMappings where
  toJSON ListStudioSessionMappings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("IdentityType" Prelude..=)
              Prelude.<$> identityType,
            ("StudioId" Prelude..=) Prelude.<$> studioId,
            ("Marker" Prelude..=) Prelude.<$> marker
          ]
      )

instance Prelude.ToPath ListStudioSessionMappings where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListStudioSessionMappings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListStudioSessionMappingsResponse' smart constructor.
data ListStudioSessionMappingsResponse = ListStudioSessionMappingsResponse'
  { -- | A list of session mapping summary objects. Each object includes session
    -- mapping details such as creation time, identity type (user or group),
    -- and Amazon EMR Studio ID.
    sessionMappings :: Prelude.Maybe [SessionMappingSummary],
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListStudioSessionMappingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionMappings', 'listStudioSessionMappingsResponse_sessionMappings' - A list of session mapping summary objects. Each object includes session
-- mapping details such as creation time, identity type (user or group),
-- and Amazon EMR Studio ID.
--
-- 'marker', 'listStudioSessionMappingsResponse_marker' - The pagination token that indicates the next set of results to retrieve.
--
-- 'httpStatus', 'listStudioSessionMappingsResponse_httpStatus' - The response's http status code.
newListStudioSessionMappingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStudioSessionMappingsResponse
newListStudioSessionMappingsResponse pHttpStatus_ =
  ListStudioSessionMappingsResponse'
    { sessionMappings =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of session mapping summary objects. Each object includes session
-- mapping details such as creation time, identity type (user or group),
-- and Amazon EMR Studio ID.
listStudioSessionMappingsResponse_sessionMappings :: Lens.Lens' ListStudioSessionMappingsResponse (Prelude.Maybe [SessionMappingSummary])
listStudioSessionMappingsResponse_sessionMappings = Lens.lens (\ListStudioSessionMappingsResponse' {sessionMappings} -> sessionMappings) (\s@ListStudioSessionMappingsResponse' {} a -> s {sessionMappings = a} :: ListStudioSessionMappingsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The pagination token that indicates the next set of results to retrieve.
listStudioSessionMappingsResponse_marker :: Lens.Lens' ListStudioSessionMappingsResponse (Prelude.Maybe Prelude.Text)
listStudioSessionMappingsResponse_marker = Lens.lens (\ListStudioSessionMappingsResponse' {marker} -> marker) (\s@ListStudioSessionMappingsResponse' {} a -> s {marker = a} :: ListStudioSessionMappingsResponse)

-- | The response's http status code.
listStudioSessionMappingsResponse_httpStatus :: Lens.Lens' ListStudioSessionMappingsResponse Prelude.Int
listStudioSessionMappingsResponse_httpStatus = Lens.lens (\ListStudioSessionMappingsResponse' {httpStatus} -> httpStatus) (\s@ListStudioSessionMappingsResponse' {} a -> s {httpStatus = a} :: ListStudioSessionMappingsResponse)

instance
  Prelude.NFData
    ListStudioSessionMappingsResponse

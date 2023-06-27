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
-- Module      : Amazonka.EMR.ListSupportedInstanceTypes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A list of the instance types that Amazon EMR supports. You can filter
-- the list by Amazon Web Services Region and Amazon EMR release.
module Amazonka.EMR.ListSupportedInstanceTypes
  ( -- * Creating a Request
    ListSupportedInstanceTypes (..),
    newListSupportedInstanceTypes,

    -- * Request Lenses
    listSupportedInstanceTypes_marker,
    listSupportedInstanceTypes_releaseLabel,

    -- * Destructuring the Response
    ListSupportedInstanceTypesResponse (..),
    newListSupportedInstanceTypesResponse,

    -- * Response Lenses
    listSupportedInstanceTypesResponse_marker,
    listSupportedInstanceTypesResponse_supportedInstanceTypes,
    listSupportedInstanceTypesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSupportedInstanceTypes' smart constructor.
data ListSupportedInstanceTypes = ListSupportedInstanceTypes'
  { -- | The pagination token that marks the next set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EMR release label determines the
    -- <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/emr-release-app-versions-6.x.html versions of open-source application packages>
    -- that Amazon EMR has installed on the cluster. Release labels are in the
    -- format @emr-x.x.x@, where x.x.x is an Amazon EMR release number such as
    -- @emr-6.10.0@. For more information about Amazon EMR releases and their
    -- included application versions and features, see the
    -- /<https://docs.aws.amazon.com/emr/latest/ReleaseGuide/emr-release-components.html Amazon EMR Release Guide>/
    -- .
    releaseLabel :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSupportedInstanceTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listSupportedInstanceTypes_marker' - The pagination token that marks the next set of results to retrieve.
--
-- 'releaseLabel', 'listSupportedInstanceTypes_releaseLabel' - The Amazon EMR release label determines the
-- <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/emr-release-app-versions-6.x.html versions of open-source application packages>
-- that Amazon EMR has installed on the cluster. Release labels are in the
-- format @emr-x.x.x@, where x.x.x is an Amazon EMR release number such as
-- @emr-6.10.0@. For more information about Amazon EMR releases and their
-- included application versions and features, see the
-- /<https://docs.aws.amazon.com/emr/latest/ReleaseGuide/emr-release-components.html Amazon EMR Release Guide>/
-- .
newListSupportedInstanceTypes ::
  -- | 'releaseLabel'
  Prelude.Text ->
  ListSupportedInstanceTypes
newListSupportedInstanceTypes pReleaseLabel_ =
  ListSupportedInstanceTypes'
    { marker =
        Prelude.Nothing,
      releaseLabel = pReleaseLabel_
    }

-- | The pagination token that marks the next set of results to retrieve.
listSupportedInstanceTypes_marker :: Lens.Lens' ListSupportedInstanceTypes (Prelude.Maybe Prelude.Text)
listSupportedInstanceTypes_marker = Lens.lens (\ListSupportedInstanceTypes' {marker} -> marker) (\s@ListSupportedInstanceTypes' {} a -> s {marker = a} :: ListSupportedInstanceTypes)

-- | The Amazon EMR release label determines the
-- <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/emr-release-app-versions-6.x.html versions of open-source application packages>
-- that Amazon EMR has installed on the cluster. Release labels are in the
-- format @emr-x.x.x@, where x.x.x is an Amazon EMR release number such as
-- @emr-6.10.0@. For more information about Amazon EMR releases and their
-- included application versions and features, see the
-- /<https://docs.aws.amazon.com/emr/latest/ReleaseGuide/emr-release-components.html Amazon EMR Release Guide>/
-- .
listSupportedInstanceTypes_releaseLabel :: Lens.Lens' ListSupportedInstanceTypes Prelude.Text
listSupportedInstanceTypes_releaseLabel = Lens.lens (\ListSupportedInstanceTypes' {releaseLabel} -> releaseLabel) (\s@ListSupportedInstanceTypes' {} a -> s {releaseLabel = a} :: ListSupportedInstanceTypes)

instance Core.AWSRequest ListSupportedInstanceTypes where
  type
    AWSResponse ListSupportedInstanceTypes =
      ListSupportedInstanceTypesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSupportedInstanceTypesResponse'
            Prelude.<$> (x Data..?> "Marker")
            Prelude.<*> ( x
                            Data..?> "SupportedInstanceTypes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSupportedInstanceTypes where
  hashWithSalt _salt ListSupportedInstanceTypes' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` releaseLabel

instance Prelude.NFData ListSupportedInstanceTypes where
  rnf ListSupportedInstanceTypes' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf releaseLabel

instance Data.ToHeaders ListSupportedInstanceTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.ListSupportedInstanceTypes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSupportedInstanceTypes where
  toJSON ListSupportedInstanceTypes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Marker" Data..=) Prelude.<$> marker,
            Prelude.Just ("ReleaseLabel" Data..= releaseLabel)
          ]
      )

instance Data.ToPath ListSupportedInstanceTypes where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSupportedInstanceTypes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSupportedInstanceTypesResponse' smart constructor.
data ListSupportedInstanceTypesResponse = ListSupportedInstanceTypesResponse'
  { -- | The pagination token that marks the next set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The list of instance types that the release specified in
    -- @ListSupportedInstanceTypesInput$ReleaseLabel@ supports, filtered by
    -- Amazon Web Services Region.
    supportedInstanceTypes :: Prelude.Maybe [SupportedInstanceType],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSupportedInstanceTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listSupportedInstanceTypesResponse_marker' - The pagination token that marks the next set of results to retrieve.
--
-- 'supportedInstanceTypes', 'listSupportedInstanceTypesResponse_supportedInstanceTypes' - The list of instance types that the release specified in
-- @ListSupportedInstanceTypesInput$ReleaseLabel@ supports, filtered by
-- Amazon Web Services Region.
--
-- 'httpStatus', 'listSupportedInstanceTypesResponse_httpStatus' - The response's http status code.
newListSupportedInstanceTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSupportedInstanceTypesResponse
newListSupportedInstanceTypesResponse pHttpStatus_ =
  ListSupportedInstanceTypesResponse'
    { marker =
        Prelude.Nothing,
      supportedInstanceTypes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token that marks the next set of results to retrieve.
listSupportedInstanceTypesResponse_marker :: Lens.Lens' ListSupportedInstanceTypesResponse (Prelude.Maybe Prelude.Text)
listSupportedInstanceTypesResponse_marker = Lens.lens (\ListSupportedInstanceTypesResponse' {marker} -> marker) (\s@ListSupportedInstanceTypesResponse' {} a -> s {marker = a} :: ListSupportedInstanceTypesResponse)

-- | The list of instance types that the release specified in
-- @ListSupportedInstanceTypesInput$ReleaseLabel@ supports, filtered by
-- Amazon Web Services Region.
listSupportedInstanceTypesResponse_supportedInstanceTypes :: Lens.Lens' ListSupportedInstanceTypesResponse (Prelude.Maybe [SupportedInstanceType])
listSupportedInstanceTypesResponse_supportedInstanceTypes = Lens.lens (\ListSupportedInstanceTypesResponse' {supportedInstanceTypes} -> supportedInstanceTypes) (\s@ListSupportedInstanceTypesResponse' {} a -> s {supportedInstanceTypes = a} :: ListSupportedInstanceTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSupportedInstanceTypesResponse_httpStatus :: Lens.Lens' ListSupportedInstanceTypesResponse Prelude.Int
listSupportedInstanceTypesResponse_httpStatus = Lens.lens (\ListSupportedInstanceTypesResponse' {httpStatus} -> httpStatus) (\s@ListSupportedInstanceTypesResponse' {} a -> s {httpStatus = a} :: ListSupportedInstanceTypesResponse)

instance
  Prelude.NFData
    ListSupportedInstanceTypesResponse
  where
  rnf ListSupportedInstanceTypesResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf supportedInstanceTypes
      `Prelude.seq` Prelude.rnf httpStatus

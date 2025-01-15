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
-- Module      : Amazonka.MediaPackageVOD.ListPackagingConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a collection of MediaPackage VOD PackagingConfiguration
-- resources.
--
-- This operation returns paginated results.
module Amazonka.MediaPackageVOD.ListPackagingConfigurations
  ( -- * Creating a Request
    ListPackagingConfigurations (..),
    newListPackagingConfigurations,

    -- * Request Lenses
    listPackagingConfigurations_maxResults,
    listPackagingConfigurations_nextToken,
    listPackagingConfigurations_packagingGroupId,

    -- * Destructuring the Response
    ListPackagingConfigurationsResponse (..),
    newListPackagingConfigurationsResponse,

    -- * Response Lenses
    listPackagingConfigurationsResponse_nextToken,
    listPackagingConfigurationsResponse_packagingConfigurations,
    listPackagingConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageVOD.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPackagingConfigurations' smart constructor.
data ListPackagingConfigurations = ListPackagingConfigurations'
  { -- | Upper bound on number of records to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token used to resume pagination from the end of a previous request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns MediaPackage VOD PackagingConfigurations associated with the
    -- specified PackagingGroup.
    packagingGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPackagingConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listPackagingConfigurations_maxResults' - Upper bound on number of records to return.
--
-- 'nextToken', 'listPackagingConfigurations_nextToken' - A token used to resume pagination from the end of a previous request.
--
-- 'packagingGroupId', 'listPackagingConfigurations_packagingGroupId' - Returns MediaPackage VOD PackagingConfigurations associated with the
-- specified PackagingGroup.
newListPackagingConfigurations ::
  ListPackagingConfigurations
newListPackagingConfigurations =
  ListPackagingConfigurations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      packagingGroupId = Prelude.Nothing
    }

-- | Upper bound on number of records to return.
listPackagingConfigurations_maxResults :: Lens.Lens' ListPackagingConfigurations (Prelude.Maybe Prelude.Natural)
listPackagingConfigurations_maxResults = Lens.lens (\ListPackagingConfigurations' {maxResults} -> maxResults) (\s@ListPackagingConfigurations' {} a -> s {maxResults = a} :: ListPackagingConfigurations)

-- | A token used to resume pagination from the end of a previous request.
listPackagingConfigurations_nextToken :: Lens.Lens' ListPackagingConfigurations (Prelude.Maybe Prelude.Text)
listPackagingConfigurations_nextToken = Lens.lens (\ListPackagingConfigurations' {nextToken} -> nextToken) (\s@ListPackagingConfigurations' {} a -> s {nextToken = a} :: ListPackagingConfigurations)

-- | Returns MediaPackage VOD PackagingConfigurations associated with the
-- specified PackagingGroup.
listPackagingConfigurations_packagingGroupId :: Lens.Lens' ListPackagingConfigurations (Prelude.Maybe Prelude.Text)
listPackagingConfigurations_packagingGroupId = Lens.lens (\ListPackagingConfigurations' {packagingGroupId} -> packagingGroupId) (\s@ListPackagingConfigurations' {} a -> s {packagingGroupId = a} :: ListPackagingConfigurations)

instance Core.AWSPager ListPackagingConfigurations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPackagingConfigurationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPackagingConfigurationsResponse_packagingConfigurations
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listPackagingConfigurations_nextToken
              Lens..~ rs
              Lens.^? listPackagingConfigurationsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListPackagingConfigurations where
  type
    AWSResponse ListPackagingConfigurations =
      ListPackagingConfigurationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPackagingConfigurationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "packagingConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPackagingConfigurations where
  hashWithSalt _salt ListPackagingConfigurations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` packagingGroupId

instance Prelude.NFData ListPackagingConfigurations where
  rnf ListPackagingConfigurations' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf packagingGroupId

instance Data.ToHeaders ListPackagingConfigurations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListPackagingConfigurations where
  toPath = Prelude.const "/packaging_configurations"

instance Data.ToQuery ListPackagingConfigurations where
  toQuery ListPackagingConfigurations' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "packagingGroupId" Data.=: packagingGroupId
      ]

-- | /See:/ 'newListPackagingConfigurationsResponse' smart constructor.
data ListPackagingConfigurationsResponse = ListPackagingConfigurationsResponse'
  { -- | A token that can be used to resume pagination from the end of the
    -- collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of MediaPackage VOD PackagingConfiguration resources.
    packagingConfigurations :: Prelude.Maybe [PackagingConfiguration],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPackagingConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPackagingConfigurationsResponse_nextToken' - A token that can be used to resume pagination from the end of the
-- collection.
--
-- 'packagingConfigurations', 'listPackagingConfigurationsResponse_packagingConfigurations' - A list of MediaPackage VOD PackagingConfiguration resources.
--
-- 'httpStatus', 'listPackagingConfigurationsResponse_httpStatus' - The response's http status code.
newListPackagingConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPackagingConfigurationsResponse
newListPackagingConfigurationsResponse pHttpStatus_ =
  ListPackagingConfigurationsResponse'
    { nextToken =
        Prelude.Nothing,
      packagingConfigurations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that can be used to resume pagination from the end of the
-- collection.
listPackagingConfigurationsResponse_nextToken :: Lens.Lens' ListPackagingConfigurationsResponse (Prelude.Maybe Prelude.Text)
listPackagingConfigurationsResponse_nextToken = Lens.lens (\ListPackagingConfigurationsResponse' {nextToken} -> nextToken) (\s@ListPackagingConfigurationsResponse' {} a -> s {nextToken = a} :: ListPackagingConfigurationsResponse)

-- | A list of MediaPackage VOD PackagingConfiguration resources.
listPackagingConfigurationsResponse_packagingConfigurations :: Lens.Lens' ListPackagingConfigurationsResponse (Prelude.Maybe [PackagingConfiguration])
listPackagingConfigurationsResponse_packagingConfigurations = Lens.lens (\ListPackagingConfigurationsResponse' {packagingConfigurations} -> packagingConfigurations) (\s@ListPackagingConfigurationsResponse' {} a -> s {packagingConfigurations = a} :: ListPackagingConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPackagingConfigurationsResponse_httpStatus :: Lens.Lens' ListPackagingConfigurationsResponse Prelude.Int
listPackagingConfigurationsResponse_httpStatus = Lens.lens (\ListPackagingConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListPackagingConfigurationsResponse' {} a -> s {httpStatus = a} :: ListPackagingConfigurationsResponse)

instance
  Prelude.NFData
    ListPackagingConfigurationsResponse
  where
  rnf ListPackagingConfigurationsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf packagingConfigurations `Prelude.seq`
        Prelude.rnf httpStatus

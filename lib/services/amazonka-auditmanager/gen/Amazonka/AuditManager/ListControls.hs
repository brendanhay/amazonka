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
-- Module      : Amazonka.AuditManager.ListControls
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of controls from Audit Manager.
module Amazonka.AuditManager.ListControls
  ( -- * Creating a Request
    ListControls (..),
    newListControls,

    -- * Request Lenses
    listControls_nextToken,
    listControls_maxResults,
    listControls_controlType,

    -- * Destructuring the Response
    ListControlsResponse (..),
    newListControlsResponse,

    -- * Response Lenses
    listControlsResponse_nextToken,
    listControlsResponse_controlMetadataList,
    listControlsResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListControls' smart constructor.
data ListControls = ListControls'
  { -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Represents the maximum number of results on a page or for an API request
    -- call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The type of control, such as a standard control or a custom control.
    controlType :: ControlType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListControls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listControls_nextToken' - The pagination token that\'s used to fetch the next set of results.
--
-- 'maxResults', 'listControls_maxResults' - Represents the maximum number of results on a page or for an API request
-- call.
--
-- 'controlType', 'listControls_controlType' - The type of control, such as a standard control or a custom control.
newListControls ::
  -- | 'controlType'
  ControlType ->
  ListControls
newListControls pControlType_ =
  ListControls'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      controlType = pControlType_
    }

-- | The pagination token that\'s used to fetch the next set of results.
listControls_nextToken :: Lens.Lens' ListControls (Prelude.Maybe Prelude.Text)
listControls_nextToken = Lens.lens (\ListControls' {nextToken} -> nextToken) (\s@ListControls' {} a -> s {nextToken = a} :: ListControls)

-- | Represents the maximum number of results on a page or for an API request
-- call.
listControls_maxResults :: Lens.Lens' ListControls (Prelude.Maybe Prelude.Natural)
listControls_maxResults = Lens.lens (\ListControls' {maxResults} -> maxResults) (\s@ListControls' {} a -> s {maxResults = a} :: ListControls)

-- | The type of control, such as a standard control or a custom control.
listControls_controlType :: Lens.Lens' ListControls ControlType
listControls_controlType = Lens.lens (\ListControls' {controlType} -> controlType) (\s@ListControls' {} a -> s {controlType = a} :: ListControls)

instance Core.AWSRequest ListControls where
  type AWSResponse ListControls = ListControlsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListControlsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "controlMetadataList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListControls where
  hashWithSalt _salt ListControls' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` controlType

instance Prelude.NFData ListControls where
  rnf ListControls' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf controlType

instance Core.ToHeaders ListControls where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListControls where
  toPath = Prelude.const "/controls"

instance Core.ToQuery ListControls where
  toQuery ListControls' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "controlType" Core.=: controlType
      ]

-- | /See:/ 'newListControlsResponse' smart constructor.
data ListControlsResponse = ListControlsResponse'
  { -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of control metadata objects that the @ListControls@ API
    -- returned.
    controlMetadataList :: Prelude.Maybe [ControlMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListControlsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listControlsResponse_nextToken' - The pagination token that\'s used to fetch the next set of results.
--
-- 'controlMetadataList', 'listControlsResponse_controlMetadataList' - The list of control metadata objects that the @ListControls@ API
-- returned.
--
-- 'httpStatus', 'listControlsResponse_httpStatus' - The response's http status code.
newListControlsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListControlsResponse
newListControlsResponse pHttpStatus_ =
  ListControlsResponse'
    { nextToken = Prelude.Nothing,
      controlMetadataList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token that\'s used to fetch the next set of results.
listControlsResponse_nextToken :: Lens.Lens' ListControlsResponse (Prelude.Maybe Prelude.Text)
listControlsResponse_nextToken = Lens.lens (\ListControlsResponse' {nextToken} -> nextToken) (\s@ListControlsResponse' {} a -> s {nextToken = a} :: ListControlsResponse)

-- | The list of control metadata objects that the @ListControls@ API
-- returned.
listControlsResponse_controlMetadataList :: Lens.Lens' ListControlsResponse (Prelude.Maybe [ControlMetadata])
listControlsResponse_controlMetadataList = Lens.lens (\ListControlsResponse' {controlMetadataList} -> controlMetadataList) (\s@ListControlsResponse' {} a -> s {controlMetadataList = a} :: ListControlsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listControlsResponse_httpStatus :: Lens.Lens' ListControlsResponse Prelude.Int
listControlsResponse_httpStatus = Lens.lens (\ListControlsResponse' {httpStatus} -> httpStatus) (\s@ListControlsResponse' {} a -> s {httpStatus = a} :: ListControlsResponse)

instance Prelude.NFData ListControlsResponse where
  rnf ListControlsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf controlMetadataList
      `Prelude.seq` Prelude.rnf httpStatus

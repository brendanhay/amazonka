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
-- Module      : Network.AWS.AuditManager.ListControls
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of controls from Audit Manager.
module Network.AWS.AuditManager.ListControls
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

import Network.AWS.AuditManager.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListControls' smart constructor.
data ListControls = ListControls'
  { -- | The pagination token used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Represents the maximum number of results per page, or per API request
    -- call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The type of control, such as standard or custom.
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
-- 'nextToken', 'listControls_nextToken' - The pagination token used to fetch the next set of results.
--
-- 'maxResults', 'listControls_maxResults' - Represents the maximum number of results per page, or per API request
-- call.
--
-- 'controlType', 'listControls_controlType' - The type of control, such as standard or custom.
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

-- | The pagination token used to fetch the next set of results.
listControls_nextToken :: Lens.Lens' ListControls (Prelude.Maybe Prelude.Text)
listControls_nextToken = Lens.lens (\ListControls' {nextToken} -> nextToken) (\s@ListControls' {} a -> s {nextToken = a} :: ListControls)

-- | Represents the maximum number of results per page, or per API request
-- call.
listControls_maxResults :: Lens.Lens' ListControls (Prelude.Maybe Prelude.Natural)
listControls_maxResults = Lens.lens (\ListControls' {maxResults} -> maxResults) (\s@ListControls' {} a -> s {maxResults = a} :: ListControls)

-- | The type of control, such as standard or custom.
listControls_controlType :: Lens.Lens' ListControls ControlType
listControls_controlType = Lens.lens (\ListControls' {controlType} -> controlType) (\s@ListControls' {} a -> s {controlType = a} :: ListControls)

instance Core.AWSRequest ListControls where
  type AWSResponse ListControls = ListControlsResponse
  request = Request.get defaultService
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

instance Prelude.Hashable ListControls

instance Prelude.NFData ListControls

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
  { -- | The pagination token used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of control metadata objects returned by the @ListControls@ API.
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
-- 'nextToken', 'listControlsResponse_nextToken' - The pagination token used to fetch the next set of results.
--
-- 'controlMetadataList', 'listControlsResponse_controlMetadataList' - The list of control metadata objects returned by the @ListControls@ API.
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

-- | The pagination token used to fetch the next set of results.
listControlsResponse_nextToken :: Lens.Lens' ListControlsResponse (Prelude.Maybe Prelude.Text)
listControlsResponse_nextToken = Lens.lens (\ListControlsResponse' {nextToken} -> nextToken) (\s@ListControlsResponse' {} a -> s {nextToken = a} :: ListControlsResponse)

-- | The list of control metadata objects returned by the @ListControls@ API.
listControlsResponse_controlMetadataList :: Lens.Lens' ListControlsResponse (Prelude.Maybe [ControlMetadata])
listControlsResponse_controlMetadataList = Lens.lens (\ListControlsResponse' {controlMetadataList} -> controlMetadataList) (\s@ListControlsResponse' {} a -> s {controlMetadataList = a} :: ListControlsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listControlsResponse_httpStatus :: Lens.Lens' ListControlsResponse Prelude.Int
listControlsResponse_httpStatus = Lens.lens (\ListControlsResponse' {httpStatus} -> httpStatus) (\s@ListControlsResponse' {} a -> s {httpStatus = a} :: ListControlsResponse)

instance Prelude.NFData ListControlsResponse

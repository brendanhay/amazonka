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
-- Module      : Network.AWS.Inspector.AddAttributesToFindings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns attributes (key and value pairs) to the findings that are
-- specified by the ARNs of the findings.
module Network.AWS.Inspector.AddAttributesToFindings
  ( -- * Creating a Request
    AddAttributesToFindings (..),
    newAddAttributesToFindings,

    -- * Request Lenses
    addAttributesToFindings_findingArns,
    addAttributesToFindings_attributes,

    -- * Destructuring the Response
    AddAttributesToFindingsResponse (..),
    newAddAttributesToFindingsResponse,

    -- * Response Lenses
    addAttributesToFindingsResponse_httpStatus,
    addAttributesToFindingsResponse_failedItems,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAddAttributesToFindings' smart constructor.
data AddAttributesToFindings = AddAttributesToFindings'
  { -- | The ARNs that specify the findings that you want to assign attributes
    -- to.
    findingArns :: Core.NonEmpty Core.Text,
    -- | The array of attributes that you want to assign to specified findings.
    attributes :: [Attribute]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddAttributesToFindings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingArns', 'addAttributesToFindings_findingArns' - The ARNs that specify the findings that you want to assign attributes
-- to.
--
-- 'attributes', 'addAttributesToFindings_attributes' - The array of attributes that you want to assign to specified findings.
newAddAttributesToFindings ::
  -- | 'findingArns'
  Core.NonEmpty Core.Text ->
  AddAttributesToFindings
newAddAttributesToFindings pFindingArns_ =
  AddAttributesToFindings'
    { findingArns =
        Lens._Coerce Lens.# pFindingArns_,
      attributes = Core.mempty
    }

-- | The ARNs that specify the findings that you want to assign attributes
-- to.
addAttributesToFindings_findingArns :: Lens.Lens' AddAttributesToFindings (Core.NonEmpty Core.Text)
addAttributesToFindings_findingArns = Lens.lens (\AddAttributesToFindings' {findingArns} -> findingArns) (\s@AddAttributesToFindings' {} a -> s {findingArns = a} :: AddAttributesToFindings) Core.. Lens._Coerce

-- | The array of attributes that you want to assign to specified findings.
addAttributesToFindings_attributes :: Lens.Lens' AddAttributesToFindings [Attribute]
addAttributesToFindings_attributes = Lens.lens (\AddAttributesToFindings' {attributes} -> attributes) (\s@AddAttributesToFindings' {} a -> s {attributes = a} :: AddAttributesToFindings) Core.. Lens._Coerce

instance Core.AWSRequest AddAttributesToFindings where
  type
    AWSResponse AddAttributesToFindings =
      AddAttributesToFindingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AddAttributesToFindingsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "failedItems" Core..!@ Core.mempty)
      )

instance Core.Hashable AddAttributesToFindings

instance Core.NFData AddAttributesToFindings

instance Core.ToHeaders AddAttributesToFindings where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.AddAttributesToFindings" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AddAttributesToFindings where
  toJSON AddAttributesToFindings' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("findingArns" Core..= findingArns),
            Core.Just ("attributes" Core..= attributes)
          ]
      )

instance Core.ToPath AddAttributesToFindings where
  toPath = Core.const "/"

instance Core.ToQuery AddAttributesToFindings where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAddAttributesToFindingsResponse' smart constructor.
data AddAttributesToFindingsResponse = AddAttributesToFindingsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Attribute details that cannot be described. An error code is provided
    -- for each failed item.
    failedItems :: Core.HashMap Core.Text FailedItemDetails
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddAttributesToFindingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'addAttributesToFindingsResponse_httpStatus' - The response's http status code.
--
-- 'failedItems', 'addAttributesToFindingsResponse_failedItems' - Attribute details that cannot be described. An error code is provided
-- for each failed item.
newAddAttributesToFindingsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AddAttributesToFindingsResponse
newAddAttributesToFindingsResponse pHttpStatus_ =
  AddAttributesToFindingsResponse'
    { httpStatus =
        pHttpStatus_,
      failedItems = Core.mempty
    }

-- | The response's http status code.
addAttributesToFindingsResponse_httpStatus :: Lens.Lens' AddAttributesToFindingsResponse Core.Int
addAttributesToFindingsResponse_httpStatus = Lens.lens (\AddAttributesToFindingsResponse' {httpStatus} -> httpStatus) (\s@AddAttributesToFindingsResponse' {} a -> s {httpStatus = a} :: AddAttributesToFindingsResponse)

-- | Attribute details that cannot be described. An error code is provided
-- for each failed item.
addAttributesToFindingsResponse_failedItems :: Lens.Lens' AddAttributesToFindingsResponse (Core.HashMap Core.Text FailedItemDetails)
addAttributesToFindingsResponse_failedItems = Lens.lens (\AddAttributesToFindingsResponse' {failedItems} -> failedItems) (\s@AddAttributesToFindingsResponse' {} a -> s {failedItems = a} :: AddAttributesToFindingsResponse) Core.. Lens._Coerce

instance Core.NFData AddAttributesToFindingsResponse

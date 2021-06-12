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
-- Module      : Network.AWS.Inspector.RemoveAttributesFromFindings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes entire attributes (key and value pairs) from the findings that
-- are specified by the ARNs of the findings where an attribute with the
-- specified key exists.
module Network.AWS.Inspector.RemoveAttributesFromFindings
  ( -- * Creating a Request
    RemoveAttributesFromFindings (..),
    newRemoveAttributesFromFindings,

    -- * Request Lenses
    removeAttributesFromFindings_findingArns,
    removeAttributesFromFindings_attributeKeys,

    -- * Destructuring the Response
    RemoveAttributesFromFindingsResponse (..),
    newRemoveAttributesFromFindingsResponse,

    -- * Response Lenses
    removeAttributesFromFindingsResponse_httpStatus,
    removeAttributesFromFindingsResponse_failedItems,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemoveAttributesFromFindings' smart constructor.
data RemoveAttributesFromFindings = RemoveAttributesFromFindings'
  { -- | The ARNs that specify the findings that you want to remove attributes
    -- from.
    findingArns :: Core.NonEmpty Core.Text,
    -- | The array of attribute keys that you want to remove from specified
    -- findings.
    attributeKeys :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveAttributesFromFindings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingArns', 'removeAttributesFromFindings_findingArns' - The ARNs that specify the findings that you want to remove attributes
-- from.
--
-- 'attributeKeys', 'removeAttributesFromFindings_attributeKeys' - The array of attribute keys that you want to remove from specified
-- findings.
newRemoveAttributesFromFindings ::
  -- | 'findingArns'
  Core.NonEmpty Core.Text ->
  RemoveAttributesFromFindings
newRemoveAttributesFromFindings pFindingArns_ =
  RemoveAttributesFromFindings'
    { findingArns =
        Lens._Coerce Lens.# pFindingArns_,
      attributeKeys = Core.mempty
    }

-- | The ARNs that specify the findings that you want to remove attributes
-- from.
removeAttributesFromFindings_findingArns :: Lens.Lens' RemoveAttributesFromFindings (Core.NonEmpty Core.Text)
removeAttributesFromFindings_findingArns = Lens.lens (\RemoveAttributesFromFindings' {findingArns} -> findingArns) (\s@RemoveAttributesFromFindings' {} a -> s {findingArns = a} :: RemoveAttributesFromFindings) Core.. Lens._Coerce

-- | The array of attribute keys that you want to remove from specified
-- findings.
removeAttributesFromFindings_attributeKeys :: Lens.Lens' RemoveAttributesFromFindings [Core.Text]
removeAttributesFromFindings_attributeKeys = Lens.lens (\RemoveAttributesFromFindings' {attributeKeys} -> attributeKeys) (\s@RemoveAttributesFromFindings' {} a -> s {attributeKeys = a} :: RemoveAttributesFromFindings) Core.. Lens._Coerce

instance Core.AWSRequest RemoveAttributesFromFindings where
  type
    AWSResponse RemoveAttributesFromFindings =
      RemoveAttributesFromFindingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveAttributesFromFindingsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "failedItems" Core..!@ Core.mempty)
      )

instance Core.Hashable RemoveAttributesFromFindings

instance Core.NFData RemoveAttributesFromFindings

instance Core.ToHeaders RemoveAttributesFromFindings where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.RemoveAttributesFromFindings" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RemoveAttributesFromFindings where
  toJSON RemoveAttributesFromFindings' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("findingArns" Core..= findingArns),
            Core.Just ("attributeKeys" Core..= attributeKeys)
          ]
      )

instance Core.ToPath RemoveAttributesFromFindings where
  toPath = Core.const "/"

instance Core.ToQuery RemoveAttributesFromFindings where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRemoveAttributesFromFindingsResponse' smart constructor.
data RemoveAttributesFromFindingsResponse = RemoveAttributesFromFindingsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Attributes details that cannot be described. An error code is provided
    -- for each failed item.
    failedItems :: Core.HashMap Core.Text FailedItemDetails
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveAttributesFromFindingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeAttributesFromFindingsResponse_httpStatus' - The response's http status code.
--
-- 'failedItems', 'removeAttributesFromFindingsResponse_failedItems' - Attributes details that cannot be described. An error code is provided
-- for each failed item.
newRemoveAttributesFromFindingsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RemoveAttributesFromFindingsResponse
newRemoveAttributesFromFindingsResponse pHttpStatus_ =
  RemoveAttributesFromFindingsResponse'
    { httpStatus =
        pHttpStatus_,
      failedItems = Core.mempty
    }

-- | The response's http status code.
removeAttributesFromFindingsResponse_httpStatus :: Lens.Lens' RemoveAttributesFromFindingsResponse Core.Int
removeAttributesFromFindingsResponse_httpStatus = Lens.lens (\RemoveAttributesFromFindingsResponse' {httpStatus} -> httpStatus) (\s@RemoveAttributesFromFindingsResponse' {} a -> s {httpStatus = a} :: RemoveAttributesFromFindingsResponse)

-- | Attributes details that cannot be described. An error code is provided
-- for each failed item.
removeAttributesFromFindingsResponse_failedItems :: Lens.Lens' RemoveAttributesFromFindingsResponse (Core.HashMap Core.Text FailedItemDetails)
removeAttributesFromFindingsResponse_failedItems = Lens.lens (\RemoveAttributesFromFindingsResponse' {failedItems} -> failedItems) (\s@RemoveAttributesFromFindingsResponse' {} a -> s {failedItems = a} :: RemoveAttributesFromFindingsResponse) Core.. Lens._Coerce

instance
  Core.NFData
    RemoveAttributesFromFindingsResponse

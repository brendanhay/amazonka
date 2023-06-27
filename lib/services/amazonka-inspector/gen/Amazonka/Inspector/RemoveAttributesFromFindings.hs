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
-- Module      : Amazonka.Inspector.RemoveAttributesFromFindings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes entire attributes (key and value pairs) from the findings that
-- are specified by the ARNs of the findings where an attribute with the
-- specified key exists.
module Amazonka.Inspector.RemoveAttributesFromFindings
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveAttributesFromFindings' smart constructor.
data RemoveAttributesFromFindings = RemoveAttributesFromFindings'
  { -- | The ARNs that specify the findings that you want to remove attributes
    -- from.
    findingArns :: Prelude.NonEmpty Prelude.Text,
    -- | The array of attribute keys that you want to remove from specified
    -- findings.
    attributeKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.NonEmpty Prelude.Text ->
  RemoveAttributesFromFindings
newRemoveAttributesFromFindings pFindingArns_ =
  RemoveAttributesFromFindings'
    { findingArns =
        Lens.coerced Lens.# pFindingArns_,
      attributeKeys = Prelude.mempty
    }

-- | The ARNs that specify the findings that you want to remove attributes
-- from.
removeAttributesFromFindings_findingArns :: Lens.Lens' RemoveAttributesFromFindings (Prelude.NonEmpty Prelude.Text)
removeAttributesFromFindings_findingArns = Lens.lens (\RemoveAttributesFromFindings' {findingArns} -> findingArns) (\s@RemoveAttributesFromFindings' {} a -> s {findingArns = a} :: RemoveAttributesFromFindings) Prelude.. Lens.coerced

-- | The array of attribute keys that you want to remove from specified
-- findings.
removeAttributesFromFindings_attributeKeys :: Lens.Lens' RemoveAttributesFromFindings [Prelude.Text]
removeAttributesFromFindings_attributeKeys = Lens.lens (\RemoveAttributesFromFindings' {attributeKeys} -> attributeKeys) (\s@RemoveAttributesFromFindings' {} a -> s {attributeKeys = a} :: RemoveAttributesFromFindings) Prelude.. Lens.coerced

instance Core.AWSRequest RemoveAttributesFromFindings where
  type
    AWSResponse RemoveAttributesFromFindings =
      RemoveAttributesFromFindingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveAttributesFromFindingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "failedItems" Core..!@ Prelude.mempty)
      )

instance
  Prelude.Hashable
    RemoveAttributesFromFindings
  where
  hashWithSalt _salt RemoveAttributesFromFindings' {..} =
    _salt
      `Prelude.hashWithSalt` findingArns
      `Prelude.hashWithSalt` attributeKeys

instance Prelude.NFData RemoveAttributesFromFindings where
  rnf RemoveAttributesFromFindings' {..} =
    Prelude.rnf findingArns
      `Prelude.seq` Prelude.rnf attributeKeys

instance Data.ToHeaders RemoveAttributesFromFindings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "InspectorService.RemoveAttributesFromFindings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RemoveAttributesFromFindings where
  toJSON RemoveAttributesFromFindings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("findingArns" Data..= findingArns),
            Prelude.Just
              ("attributeKeys" Data..= attributeKeys)
          ]
      )

instance Data.ToPath RemoveAttributesFromFindings where
  toPath = Prelude.const "/"

instance Data.ToQuery RemoveAttributesFromFindings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveAttributesFromFindingsResponse' smart constructor.
data RemoveAttributesFromFindingsResponse = RemoveAttributesFromFindingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Attributes details that cannot be described. An error code is provided
    -- for each failed item.
    failedItems :: Prelude.HashMap Prelude.Text FailedItemDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  RemoveAttributesFromFindingsResponse
newRemoveAttributesFromFindingsResponse pHttpStatus_ =
  RemoveAttributesFromFindingsResponse'
    { httpStatus =
        pHttpStatus_,
      failedItems = Prelude.mempty
    }

-- | The response's http status code.
removeAttributesFromFindingsResponse_httpStatus :: Lens.Lens' RemoveAttributesFromFindingsResponse Prelude.Int
removeAttributesFromFindingsResponse_httpStatus = Lens.lens (\RemoveAttributesFromFindingsResponse' {httpStatus} -> httpStatus) (\s@RemoveAttributesFromFindingsResponse' {} a -> s {httpStatus = a} :: RemoveAttributesFromFindingsResponse)

-- | Attributes details that cannot be described. An error code is provided
-- for each failed item.
removeAttributesFromFindingsResponse_failedItems :: Lens.Lens' RemoveAttributesFromFindingsResponse (Prelude.HashMap Prelude.Text FailedItemDetails)
removeAttributesFromFindingsResponse_failedItems = Lens.lens (\RemoveAttributesFromFindingsResponse' {failedItems} -> failedItems) (\s@RemoveAttributesFromFindingsResponse' {} a -> s {failedItems = a} :: RemoveAttributesFromFindingsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    RemoveAttributesFromFindingsResponse
  where
  rnf RemoveAttributesFromFindingsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf failedItems

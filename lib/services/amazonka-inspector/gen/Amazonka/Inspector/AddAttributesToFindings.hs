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
-- Module      : Amazonka.Inspector.AddAttributesToFindings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns attributes (key and value pairs) to the findings that are
-- specified by the ARNs of the findings.
module Amazonka.Inspector.AddAttributesToFindings
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddAttributesToFindings' smart constructor.
data AddAttributesToFindings = AddAttributesToFindings'
  { -- | The ARNs that specify the findings that you want to assign attributes
    -- to.
    findingArns :: Prelude.NonEmpty Prelude.Text,
    -- | The array of attributes that you want to assign to specified findings.
    attributes :: [Attribute]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.NonEmpty Prelude.Text ->
  AddAttributesToFindings
newAddAttributesToFindings pFindingArns_ =
  AddAttributesToFindings'
    { findingArns =
        Lens.coerced Lens.# pFindingArns_,
      attributes = Prelude.mempty
    }

-- | The ARNs that specify the findings that you want to assign attributes
-- to.
addAttributesToFindings_findingArns :: Lens.Lens' AddAttributesToFindings (Prelude.NonEmpty Prelude.Text)
addAttributesToFindings_findingArns = Lens.lens (\AddAttributesToFindings' {findingArns} -> findingArns) (\s@AddAttributesToFindings' {} a -> s {findingArns = a} :: AddAttributesToFindings) Prelude.. Lens.coerced

-- | The array of attributes that you want to assign to specified findings.
addAttributesToFindings_attributes :: Lens.Lens' AddAttributesToFindings [Attribute]
addAttributesToFindings_attributes = Lens.lens (\AddAttributesToFindings' {attributes} -> attributes) (\s@AddAttributesToFindings' {} a -> s {attributes = a} :: AddAttributesToFindings) Prelude.. Lens.coerced

instance Core.AWSRequest AddAttributesToFindings where
  type
    AWSResponse AddAttributesToFindings =
      AddAttributesToFindingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddAttributesToFindingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "failedItems" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable AddAttributesToFindings where
  hashWithSalt _salt AddAttributesToFindings' {..} =
    _salt
      `Prelude.hashWithSalt` findingArns
      `Prelude.hashWithSalt` attributes

instance Prelude.NFData AddAttributesToFindings where
  rnf AddAttributesToFindings' {..} =
    Prelude.rnf findingArns `Prelude.seq`
      Prelude.rnf attributes

instance Data.ToHeaders AddAttributesToFindings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "InspectorService.AddAttributesToFindings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddAttributesToFindings where
  toJSON AddAttributesToFindings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("findingArns" Data..= findingArns),
            Prelude.Just ("attributes" Data..= attributes)
          ]
      )

instance Data.ToPath AddAttributesToFindings where
  toPath = Prelude.const "/"

instance Data.ToQuery AddAttributesToFindings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddAttributesToFindingsResponse' smart constructor.
data AddAttributesToFindingsResponse = AddAttributesToFindingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Attribute details that cannot be described. An error code is provided
    -- for each failed item.
    failedItems :: Prelude.HashMap Prelude.Text FailedItemDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  AddAttributesToFindingsResponse
newAddAttributesToFindingsResponse pHttpStatus_ =
  AddAttributesToFindingsResponse'
    { httpStatus =
        pHttpStatus_,
      failedItems = Prelude.mempty
    }

-- | The response's http status code.
addAttributesToFindingsResponse_httpStatus :: Lens.Lens' AddAttributesToFindingsResponse Prelude.Int
addAttributesToFindingsResponse_httpStatus = Lens.lens (\AddAttributesToFindingsResponse' {httpStatus} -> httpStatus) (\s@AddAttributesToFindingsResponse' {} a -> s {httpStatus = a} :: AddAttributesToFindingsResponse)

-- | Attribute details that cannot be described. An error code is provided
-- for each failed item.
addAttributesToFindingsResponse_failedItems :: Lens.Lens' AddAttributesToFindingsResponse (Prelude.HashMap Prelude.Text FailedItemDetails)
addAttributesToFindingsResponse_failedItems = Lens.lens (\AddAttributesToFindingsResponse' {failedItems} -> failedItems) (\s@AddAttributesToFindingsResponse' {} a -> s {failedItems = a} :: AddAttributesToFindingsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    AddAttributesToFindingsResponse
  where
  rnf AddAttributesToFindingsResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf failedItems

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
-- Module      : Amazonka.SSMIncidents.UpdateRelatedItems
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add or remove related items from the related items tab of an incident
-- record.
module Amazonka.SSMIncidents.UpdateRelatedItems
  ( -- * Creating a Request
    UpdateRelatedItems (..),
    newUpdateRelatedItems,

    -- * Request Lenses
    updateRelatedItems_clientToken,
    updateRelatedItems_incidentRecordArn,
    updateRelatedItems_relatedItemsUpdate,

    -- * Destructuring the Response
    UpdateRelatedItemsResponse (..),
    newUpdateRelatedItemsResponse,

    -- * Response Lenses
    updateRelatedItemsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newUpdateRelatedItems' smart constructor.
data UpdateRelatedItems = UpdateRelatedItems'
  { -- | A token ensuring that the operation is called only once with the
    -- specified details.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the incident record containing the
    -- related items you are updating.
    incidentRecordArn :: Prelude.Text,
    -- | Details about the item you are adding or deleting.
    relatedItemsUpdate :: RelatedItemsUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRelatedItems' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateRelatedItems_clientToken' - A token ensuring that the operation is called only once with the
-- specified details.
--
-- 'incidentRecordArn', 'updateRelatedItems_incidentRecordArn' - The Amazon Resource Name (ARN) of the incident record containing the
-- related items you are updating.
--
-- 'relatedItemsUpdate', 'updateRelatedItems_relatedItemsUpdate' - Details about the item you are adding or deleting.
newUpdateRelatedItems ::
  -- | 'incidentRecordArn'
  Prelude.Text ->
  -- | 'relatedItemsUpdate'
  RelatedItemsUpdate ->
  UpdateRelatedItems
newUpdateRelatedItems
  pIncidentRecordArn_
  pRelatedItemsUpdate_ =
    UpdateRelatedItems'
      { clientToken = Prelude.Nothing,
        incidentRecordArn = pIncidentRecordArn_,
        relatedItemsUpdate = pRelatedItemsUpdate_
      }

-- | A token ensuring that the operation is called only once with the
-- specified details.
updateRelatedItems_clientToken :: Lens.Lens' UpdateRelatedItems (Prelude.Maybe Prelude.Text)
updateRelatedItems_clientToken = Lens.lens (\UpdateRelatedItems' {clientToken} -> clientToken) (\s@UpdateRelatedItems' {} a -> s {clientToken = a} :: UpdateRelatedItems)

-- | The Amazon Resource Name (ARN) of the incident record containing the
-- related items you are updating.
updateRelatedItems_incidentRecordArn :: Lens.Lens' UpdateRelatedItems Prelude.Text
updateRelatedItems_incidentRecordArn = Lens.lens (\UpdateRelatedItems' {incidentRecordArn} -> incidentRecordArn) (\s@UpdateRelatedItems' {} a -> s {incidentRecordArn = a} :: UpdateRelatedItems)

-- | Details about the item you are adding or deleting.
updateRelatedItems_relatedItemsUpdate :: Lens.Lens' UpdateRelatedItems RelatedItemsUpdate
updateRelatedItems_relatedItemsUpdate = Lens.lens (\UpdateRelatedItems' {relatedItemsUpdate} -> relatedItemsUpdate) (\s@UpdateRelatedItems' {} a -> s {relatedItemsUpdate = a} :: UpdateRelatedItems)

instance Core.AWSRequest UpdateRelatedItems where
  type
    AWSResponse UpdateRelatedItems =
      UpdateRelatedItemsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateRelatedItemsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRelatedItems where
  hashWithSalt _salt UpdateRelatedItems' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` incidentRecordArn
      `Prelude.hashWithSalt` relatedItemsUpdate

instance Prelude.NFData UpdateRelatedItems where
  rnf UpdateRelatedItems' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf incidentRecordArn
      `Prelude.seq` Prelude.rnf relatedItemsUpdate

instance Data.ToHeaders UpdateRelatedItems where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRelatedItems where
  toJSON UpdateRelatedItems' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just
              ("incidentRecordArn" Data..= incidentRecordArn),
            Prelude.Just
              ("relatedItemsUpdate" Data..= relatedItemsUpdate)
          ]
      )

instance Data.ToPath UpdateRelatedItems where
  toPath = Prelude.const "/updateRelatedItems"

instance Data.ToQuery UpdateRelatedItems where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRelatedItemsResponse' smart constructor.
data UpdateRelatedItemsResponse = UpdateRelatedItemsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRelatedItemsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateRelatedItemsResponse_httpStatus' - The response's http status code.
newUpdateRelatedItemsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRelatedItemsResponse
newUpdateRelatedItemsResponse pHttpStatus_ =
  UpdateRelatedItemsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateRelatedItemsResponse_httpStatus :: Lens.Lens' UpdateRelatedItemsResponse Prelude.Int
updateRelatedItemsResponse_httpStatus = Lens.lens (\UpdateRelatedItemsResponse' {httpStatus} -> httpStatus) (\s@UpdateRelatedItemsResponse' {} a -> s {httpStatus = a} :: UpdateRelatedItemsResponse)

instance Prelude.NFData UpdateRelatedItemsResponse where
  rnf UpdateRelatedItemsResponse' {..} =
    Prelude.rnf httpStatus

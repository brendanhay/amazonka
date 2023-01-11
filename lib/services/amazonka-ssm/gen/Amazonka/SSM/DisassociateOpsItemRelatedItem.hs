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
-- Module      : Amazonka.SSM.DisassociateOpsItemRelatedItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the association between an OpsItem and a related item. For
-- example, this API operation can delete an Incident Manager incident from
-- an OpsItem. Incident Manager is a capability of Amazon Web Services
-- Systems Manager.
module Amazonka.SSM.DisassociateOpsItemRelatedItem
  ( -- * Creating a Request
    DisassociateOpsItemRelatedItem (..),
    newDisassociateOpsItemRelatedItem,

    -- * Request Lenses
    disassociateOpsItemRelatedItem_opsItemId,
    disassociateOpsItemRelatedItem_associationId,

    -- * Destructuring the Response
    DisassociateOpsItemRelatedItemResponse (..),
    newDisassociateOpsItemRelatedItemResponse,

    -- * Response Lenses
    disassociateOpsItemRelatedItemResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDisassociateOpsItemRelatedItem' smart constructor.
data DisassociateOpsItemRelatedItem = DisassociateOpsItemRelatedItem'
  { -- | The ID of the OpsItem for which you want to delete an association
    -- between the OpsItem and a related item.
    opsItemId :: Prelude.Text,
    -- | The ID of the association for which you want to delete an association
    -- between the OpsItem and a related item.
    associationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateOpsItemRelatedItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'opsItemId', 'disassociateOpsItemRelatedItem_opsItemId' - The ID of the OpsItem for which you want to delete an association
-- between the OpsItem and a related item.
--
-- 'associationId', 'disassociateOpsItemRelatedItem_associationId' - The ID of the association for which you want to delete an association
-- between the OpsItem and a related item.
newDisassociateOpsItemRelatedItem ::
  -- | 'opsItemId'
  Prelude.Text ->
  -- | 'associationId'
  Prelude.Text ->
  DisassociateOpsItemRelatedItem
newDisassociateOpsItemRelatedItem
  pOpsItemId_
  pAssociationId_ =
    DisassociateOpsItemRelatedItem'
      { opsItemId =
          pOpsItemId_,
        associationId = pAssociationId_
      }

-- | The ID of the OpsItem for which you want to delete an association
-- between the OpsItem and a related item.
disassociateOpsItemRelatedItem_opsItemId :: Lens.Lens' DisassociateOpsItemRelatedItem Prelude.Text
disassociateOpsItemRelatedItem_opsItemId = Lens.lens (\DisassociateOpsItemRelatedItem' {opsItemId} -> opsItemId) (\s@DisassociateOpsItemRelatedItem' {} a -> s {opsItemId = a} :: DisassociateOpsItemRelatedItem)

-- | The ID of the association for which you want to delete an association
-- between the OpsItem and a related item.
disassociateOpsItemRelatedItem_associationId :: Lens.Lens' DisassociateOpsItemRelatedItem Prelude.Text
disassociateOpsItemRelatedItem_associationId = Lens.lens (\DisassociateOpsItemRelatedItem' {associationId} -> associationId) (\s@DisassociateOpsItemRelatedItem' {} a -> s {associationId = a} :: DisassociateOpsItemRelatedItem)

instance
  Core.AWSRequest
    DisassociateOpsItemRelatedItem
  where
  type
    AWSResponse DisassociateOpsItemRelatedItem =
      DisassociateOpsItemRelatedItemResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateOpsItemRelatedItemResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateOpsItemRelatedItem
  where
  hashWithSalt
    _salt
    DisassociateOpsItemRelatedItem' {..} =
      _salt `Prelude.hashWithSalt` opsItemId
        `Prelude.hashWithSalt` associationId

instance
  Prelude.NFData
    DisassociateOpsItemRelatedItem
  where
  rnf DisassociateOpsItemRelatedItem' {..} =
    Prelude.rnf opsItemId
      `Prelude.seq` Prelude.rnf associationId

instance
  Data.ToHeaders
    DisassociateOpsItemRelatedItem
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.DisassociateOpsItemRelatedItem" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateOpsItemRelatedItem where
  toJSON DisassociateOpsItemRelatedItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("OpsItemId" Data..= opsItemId),
            Prelude.Just
              ("AssociationId" Data..= associationId)
          ]
      )

instance Data.ToPath DisassociateOpsItemRelatedItem where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateOpsItemRelatedItem where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateOpsItemRelatedItemResponse' smart constructor.
data DisassociateOpsItemRelatedItemResponse = DisassociateOpsItemRelatedItemResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateOpsItemRelatedItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateOpsItemRelatedItemResponse_httpStatus' - The response's http status code.
newDisassociateOpsItemRelatedItemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateOpsItemRelatedItemResponse
newDisassociateOpsItemRelatedItemResponse
  pHttpStatus_ =
    DisassociateOpsItemRelatedItemResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateOpsItemRelatedItemResponse_httpStatus :: Lens.Lens' DisassociateOpsItemRelatedItemResponse Prelude.Int
disassociateOpsItemRelatedItemResponse_httpStatus = Lens.lens (\DisassociateOpsItemRelatedItemResponse' {httpStatus} -> httpStatus) (\s@DisassociateOpsItemRelatedItemResponse' {} a -> s {httpStatus = a} :: DisassociateOpsItemRelatedItemResponse)

instance
  Prelude.NFData
    DisassociateOpsItemRelatedItemResponse
  where
  rnf DisassociateOpsItemRelatedItemResponse' {..} =
    Prelude.rnf httpStatus

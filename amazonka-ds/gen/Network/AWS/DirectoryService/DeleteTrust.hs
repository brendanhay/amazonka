{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DirectoryService.DeleteTrust
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing trust relationship between your AWS Managed
-- Microsoft AD directory and an external domain.
module Network.AWS.DirectoryService.DeleteTrust
  ( -- * Creating a Request
    DeleteTrust (..),
    newDeleteTrust,

    -- * Request Lenses
    deleteTrust_deleteAssociatedConditionalForwarder,
    deleteTrust_trustId,

    -- * Destructuring the Response
    DeleteTrustResponse (..),
    newDeleteTrustResponse,

    -- * Response Lenses
    deleteTrustResponse_trustId,
    deleteTrustResponse_httpStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Deletes the local side of an existing trust relationship between the AWS
-- Managed Microsoft AD directory and the external domain.
--
-- /See:/ 'newDeleteTrust' smart constructor.
data DeleteTrust = DeleteTrust'
  { -- | Delete a conditional forwarder as part of a DeleteTrustRequest.
    deleteAssociatedConditionalForwarder :: Prelude.Maybe Prelude.Bool,
    -- | The Trust ID of the trust relationship to be deleted.
    trustId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTrust' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteAssociatedConditionalForwarder', 'deleteTrust_deleteAssociatedConditionalForwarder' - Delete a conditional forwarder as part of a DeleteTrustRequest.
--
-- 'trustId', 'deleteTrust_trustId' - The Trust ID of the trust relationship to be deleted.
newDeleteTrust ::
  -- | 'trustId'
  Prelude.Text ->
  DeleteTrust
newDeleteTrust pTrustId_ =
  DeleteTrust'
    { deleteAssociatedConditionalForwarder =
        Prelude.Nothing,
      trustId = pTrustId_
    }

-- | Delete a conditional forwarder as part of a DeleteTrustRequest.
deleteTrust_deleteAssociatedConditionalForwarder :: Lens.Lens' DeleteTrust (Prelude.Maybe Prelude.Bool)
deleteTrust_deleteAssociatedConditionalForwarder = Lens.lens (\DeleteTrust' {deleteAssociatedConditionalForwarder} -> deleteAssociatedConditionalForwarder) (\s@DeleteTrust' {} a -> s {deleteAssociatedConditionalForwarder = a} :: DeleteTrust)

-- | The Trust ID of the trust relationship to be deleted.
deleteTrust_trustId :: Lens.Lens' DeleteTrust Prelude.Text
deleteTrust_trustId = Lens.lens (\DeleteTrust' {trustId} -> trustId) (\s@DeleteTrust' {} a -> s {trustId = a} :: DeleteTrust)

instance Prelude.AWSRequest DeleteTrust where
  type Rs DeleteTrust = DeleteTrustResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTrustResponse'
            Prelude.<$> (x Prelude..?> "TrustId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTrust

instance Prelude.NFData DeleteTrust

instance Prelude.ToHeaders DeleteTrust where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.DeleteTrust" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteTrust where
  toJSON DeleteTrust' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DeleteAssociatedConditionalForwarder" Prelude..=)
              Prelude.<$> deleteAssociatedConditionalForwarder,
            Prelude.Just ("TrustId" Prelude..= trustId)
          ]
      )

instance Prelude.ToPath DeleteTrust where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteTrust where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a DeleteTrust request.
--
-- /See:/ 'newDeleteTrustResponse' smart constructor.
data DeleteTrustResponse = DeleteTrustResponse'
  { -- | The Trust ID of the trust relationship that was deleted.
    trustId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTrustResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trustId', 'deleteTrustResponse_trustId' - The Trust ID of the trust relationship that was deleted.
--
-- 'httpStatus', 'deleteTrustResponse_httpStatus' - The response's http status code.
newDeleteTrustResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTrustResponse
newDeleteTrustResponse pHttpStatus_ =
  DeleteTrustResponse'
    { trustId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Trust ID of the trust relationship that was deleted.
deleteTrustResponse_trustId :: Lens.Lens' DeleteTrustResponse (Prelude.Maybe Prelude.Text)
deleteTrustResponse_trustId = Lens.lens (\DeleteTrustResponse' {trustId} -> trustId) (\s@DeleteTrustResponse' {} a -> s {trustId = a} :: DeleteTrustResponse)

-- | The response's http status code.
deleteTrustResponse_httpStatus :: Lens.Lens' DeleteTrustResponse Prelude.Int
deleteTrustResponse_httpStatus = Lens.lens (\DeleteTrustResponse' {httpStatus} -> httpStatus) (\s@DeleteTrustResponse' {} a -> s {httpStatus = a} :: DeleteTrustResponse)

instance Prelude.NFData DeleteTrustResponse

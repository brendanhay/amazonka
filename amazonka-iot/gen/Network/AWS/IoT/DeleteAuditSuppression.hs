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
-- Module      : Network.AWS.IoT.DeleteAuditSuppression
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Device Defender audit suppression.
module Network.AWS.IoT.DeleteAuditSuppression
  ( -- * Creating a Request
    DeleteAuditSuppression (..),
    newDeleteAuditSuppression,

    -- * Request Lenses
    deleteAuditSuppression_checkName,
    deleteAuditSuppression_resourceIdentifier,

    -- * Destructuring the Response
    DeleteAuditSuppressionResponse (..),
    newDeleteAuditSuppressionResponse,

    -- * Response Lenses
    deleteAuditSuppressionResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAuditSuppression' smart constructor.
data DeleteAuditSuppression = DeleteAuditSuppression'
  { checkName :: Prelude.Text,
    resourceIdentifier :: ResourceIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAuditSuppression' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checkName', 'deleteAuditSuppression_checkName' - Undocumented member.
--
-- 'resourceIdentifier', 'deleteAuditSuppression_resourceIdentifier' - Undocumented member.
newDeleteAuditSuppression ::
  -- | 'checkName'
  Prelude.Text ->
  -- | 'resourceIdentifier'
  ResourceIdentifier ->
  DeleteAuditSuppression
newDeleteAuditSuppression
  pCheckName_
  pResourceIdentifier_ =
    DeleteAuditSuppression'
      { checkName = pCheckName_,
        resourceIdentifier = pResourceIdentifier_
      }

-- | Undocumented member.
deleteAuditSuppression_checkName :: Lens.Lens' DeleteAuditSuppression Prelude.Text
deleteAuditSuppression_checkName = Lens.lens (\DeleteAuditSuppression' {checkName} -> checkName) (\s@DeleteAuditSuppression' {} a -> s {checkName = a} :: DeleteAuditSuppression)

-- | Undocumented member.
deleteAuditSuppression_resourceIdentifier :: Lens.Lens' DeleteAuditSuppression ResourceIdentifier
deleteAuditSuppression_resourceIdentifier = Lens.lens (\DeleteAuditSuppression' {resourceIdentifier} -> resourceIdentifier) (\s@DeleteAuditSuppression' {} a -> s {resourceIdentifier = a} :: DeleteAuditSuppression)

instance Prelude.AWSRequest DeleteAuditSuppression where
  type
    Rs DeleteAuditSuppression =
      DeleteAuditSuppressionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAuditSuppressionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAuditSuppression

instance Prelude.NFData DeleteAuditSuppression

instance Prelude.ToHeaders DeleteAuditSuppression where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON DeleteAuditSuppression where
  toJSON DeleteAuditSuppression' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("checkName" Prelude..= checkName),
            Prelude.Just
              ( "resourceIdentifier"
                  Prelude..= resourceIdentifier
              )
          ]
      )

instance Prelude.ToPath DeleteAuditSuppression where
  toPath = Prelude.const "/audit/suppressions/delete"

instance Prelude.ToQuery DeleteAuditSuppression where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAuditSuppressionResponse' smart constructor.
data DeleteAuditSuppressionResponse = DeleteAuditSuppressionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAuditSuppressionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAuditSuppressionResponse_httpStatus' - The response's http status code.
newDeleteAuditSuppressionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAuditSuppressionResponse
newDeleteAuditSuppressionResponse pHttpStatus_ =
  DeleteAuditSuppressionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAuditSuppressionResponse_httpStatus :: Lens.Lens' DeleteAuditSuppressionResponse Prelude.Int
deleteAuditSuppressionResponse_httpStatus = Lens.lens (\DeleteAuditSuppressionResponse' {httpStatus} -> httpStatus) (\s@DeleteAuditSuppressionResponse' {} a -> s {httpStatus = a} :: DeleteAuditSuppressionResponse)

instance
  Prelude.NFData
    DeleteAuditSuppressionResponse

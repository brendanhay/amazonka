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
-- Module      : Amazonka.FraudDetector.DeleteLabel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a label.
--
-- You cannot delete labels that are included in an event type in Amazon
-- Fraud Detector.
--
-- You cannot delete a label assigned to an event ID. You must first delete
-- the relevant event ID.
--
-- When you delete a label, Amazon Fraud Detector permanently deletes that
-- label and the data is no longer stored in Amazon Fraud Detector.
module Amazonka.FraudDetector.DeleteLabel
  ( -- * Creating a Request
    DeleteLabel (..),
    newDeleteLabel,

    -- * Request Lenses
    deleteLabel_name,

    -- * Destructuring the Response
    DeleteLabelResponse (..),
    newDeleteLabelResponse,

    -- * Response Lenses
    deleteLabelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLabel' smart constructor.
data DeleteLabel = DeleteLabel'
  { -- | The name of the label to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLabel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteLabel_name' - The name of the label to delete.
newDeleteLabel ::
  -- | 'name'
  Prelude.Text ->
  DeleteLabel
newDeleteLabel pName_ = DeleteLabel' {name = pName_}

-- | The name of the label to delete.
deleteLabel_name :: Lens.Lens' DeleteLabel Prelude.Text
deleteLabel_name = Lens.lens (\DeleteLabel' {name} -> name) (\s@DeleteLabel' {} a -> s {name = a} :: DeleteLabel)

instance Core.AWSRequest DeleteLabel where
  type AWSResponse DeleteLabel = DeleteLabelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLabelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLabel where
  hashWithSalt _salt DeleteLabel' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteLabel where
  rnf DeleteLabel' {..} = Prelude.rnf name

instance Core.ToHeaders DeleteLabel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHawksNestServiceFacade.DeleteLabel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteLabel where
  toJSON DeleteLabel' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Core..= name)]
      )

instance Core.ToPath DeleteLabel where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteLabel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLabelResponse' smart constructor.
data DeleteLabelResponse = DeleteLabelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLabelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteLabelResponse_httpStatus' - The response's http status code.
newDeleteLabelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLabelResponse
newDeleteLabelResponse pHttpStatus_ =
  DeleteLabelResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteLabelResponse_httpStatus :: Lens.Lens' DeleteLabelResponse Prelude.Int
deleteLabelResponse_httpStatus = Lens.lens (\DeleteLabelResponse' {httpStatus} -> httpStatus) (\s@DeleteLabelResponse' {} a -> s {httpStatus = a} :: DeleteLabelResponse)

instance Prelude.NFData DeleteLabelResponse where
  rnf DeleteLabelResponse' {..} = Prelude.rnf httpStatus

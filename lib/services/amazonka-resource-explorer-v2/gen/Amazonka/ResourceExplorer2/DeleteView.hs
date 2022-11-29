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
-- Module      : Amazonka.ResourceExplorer2.DeleteView
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified view.
--
-- If the specified view is the default view for its Amazon Web Services
-- Region, then all Search operations in that Region must explicitly
-- specify the view to use until you configure a new default by calling the
-- AssociateDefaultView operation.
module Amazonka.ResourceExplorer2.DeleteView
  ( -- * Creating a Request
    DeleteView (..),
    newDeleteView,

    -- * Request Lenses
    deleteView_viewArn,

    -- * Destructuring the Response
    DeleteViewResponse (..),
    newDeleteViewResponse,

    -- * Response Lenses
    deleteViewResponse_viewArn,
    deleteViewResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceExplorer2.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteView' smart constructor.
data DeleteView = DeleteView'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
    -- of the view that you want to delete.
    viewArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteView' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'viewArn', 'deleteView_viewArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the view that you want to delete.
newDeleteView ::
  -- | 'viewArn'
  Prelude.Text ->
  DeleteView
newDeleteView pViewArn_ =
  DeleteView' {viewArn = pViewArn_}

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the view that you want to delete.
deleteView_viewArn :: Lens.Lens' DeleteView Prelude.Text
deleteView_viewArn = Lens.lens (\DeleteView' {viewArn} -> viewArn) (\s@DeleteView' {} a -> s {viewArn = a} :: DeleteView)

instance Core.AWSRequest DeleteView where
  type AWSResponse DeleteView = DeleteViewResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteViewResponse'
            Prelude.<$> (x Core..?> "ViewArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteView where
  hashWithSalt _salt DeleteView' {..} =
    _salt `Prelude.hashWithSalt` viewArn

instance Prelude.NFData DeleteView where
  rnf DeleteView' {..} = Prelude.rnf viewArn

instance Core.ToHeaders DeleteView where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteView where
  toJSON DeleteView' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ViewArn" Core..= viewArn)]
      )

instance Core.ToPath DeleteView where
  toPath = Prelude.const "/DeleteView"

instance Core.ToQuery DeleteView where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteViewResponse' smart constructor.
data DeleteViewResponse = DeleteViewResponse'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
    -- of the view that you successfully deleted.
    viewArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteViewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'viewArn', 'deleteViewResponse_viewArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the view that you successfully deleted.
--
-- 'httpStatus', 'deleteViewResponse_httpStatus' - The response's http status code.
newDeleteViewResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteViewResponse
newDeleteViewResponse pHttpStatus_ =
  DeleteViewResponse'
    { viewArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the view that you successfully deleted.
deleteViewResponse_viewArn :: Lens.Lens' DeleteViewResponse (Prelude.Maybe Prelude.Text)
deleteViewResponse_viewArn = Lens.lens (\DeleteViewResponse' {viewArn} -> viewArn) (\s@DeleteViewResponse' {} a -> s {viewArn = a} :: DeleteViewResponse)

-- | The response's http status code.
deleteViewResponse_httpStatus :: Lens.Lens' DeleteViewResponse Prelude.Int
deleteViewResponse_httpStatus = Lens.lens (\DeleteViewResponse' {httpStatus} -> httpStatus) (\s@DeleteViewResponse' {} a -> s {httpStatus = a} :: DeleteViewResponse)

instance Prelude.NFData DeleteViewResponse where
  rnf DeleteViewResponse' {..} =
    Prelude.rnf viewArn
      `Prelude.seq` Prelude.rnf httpStatus

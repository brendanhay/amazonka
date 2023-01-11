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
-- Module      : Amazonka.ResourceExplorer2.AssociateDefaultView
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified view as the default for the Amazon Web Services
-- Region in which you call this operation. When a user performs a Search
-- that doesn\'t explicitly specify which view to use, then Amazon Web
-- Services Resource Explorer automatically chooses this default view for
-- searches performed in this Amazon Web Services Region.
--
-- If an Amazon Web Services Region doesn\'t have a default view
-- configured, then users must explicitly specify a view with every
-- @Search@ operation performed in that Region.
module Amazonka.ResourceExplorer2.AssociateDefaultView
  ( -- * Creating a Request
    AssociateDefaultView (..),
    newAssociateDefaultView,

    -- * Request Lenses
    associateDefaultView_viewArn,

    -- * Destructuring the Response
    AssociateDefaultViewResponse (..),
    newAssociateDefaultViewResponse,

    -- * Response Lenses
    associateDefaultViewResponse_viewArn,
    associateDefaultViewResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceExplorer2.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateDefaultView' smart constructor.
data AssociateDefaultView = AssociateDefaultView'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
    -- of the view to set as the default for the Amazon Web Services Region and
    -- Amazon Web Services account in which you call this operation. The
    -- specified view must already exist in the called Region.
    viewArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateDefaultView' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'viewArn', 'associateDefaultView_viewArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the view to set as the default for the Amazon Web Services Region and
-- Amazon Web Services account in which you call this operation. The
-- specified view must already exist in the called Region.
newAssociateDefaultView ::
  -- | 'viewArn'
  Prelude.Text ->
  AssociateDefaultView
newAssociateDefaultView pViewArn_ =
  AssociateDefaultView' {viewArn = pViewArn_}

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the view to set as the default for the Amazon Web Services Region and
-- Amazon Web Services account in which you call this operation. The
-- specified view must already exist in the called Region.
associateDefaultView_viewArn :: Lens.Lens' AssociateDefaultView Prelude.Text
associateDefaultView_viewArn = Lens.lens (\AssociateDefaultView' {viewArn} -> viewArn) (\s@AssociateDefaultView' {} a -> s {viewArn = a} :: AssociateDefaultView)

instance Core.AWSRequest AssociateDefaultView where
  type
    AWSResponse AssociateDefaultView =
      AssociateDefaultViewResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateDefaultViewResponse'
            Prelude.<$> (x Data..?> "ViewArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateDefaultView where
  hashWithSalt _salt AssociateDefaultView' {..} =
    _salt `Prelude.hashWithSalt` viewArn

instance Prelude.NFData AssociateDefaultView where
  rnf AssociateDefaultView' {..} = Prelude.rnf viewArn

instance Data.ToHeaders AssociateDefaultView where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateDefaultView where
  toJSON AssociateDefaultView' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ViewArn" Data..= viewArn)]
      )

instance Data.ToPath AssociateDefaultView where
  toPath = Prelude.const "/AssociateDefaultView"

instance Data.ToQuery AssociateDefaultView where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateDefaultViewResponse' smart constructor.
data AssociateDefaultViewResponse = AssociateDefaultViewResponse'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
    -- of the view that the operation set as the default for queries made in
    -- the Amazon Web Services Region and Amazon Web Services account in which
    -- you called this operation.
    viewArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateDefaultViewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'viewArn', 'associateDefaultViewResponse_viewArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the view that the operation set as the default for queries made in
-- the Amazon Web Services Region and Amazon Web Services account in which
-- you called this operation.
--
-- 'httpStatus', 'associateDefaultViewResponse_httpStatus' - The response's http status code.
newAssociateDefaultViewResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateDefaultViewResponse
newAssociateDefaultViewResponse pHttpStatus_ =
  AssociateDefaultViewResponse'
    { viewArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the view that the operation set as the default for queries made in
-- the Amazon Web Services Region and Amazon Web Services account in which
-- you called this operation.
associateDefaultViewResponse_viewArn :: Lens.Lens' AssociateDefaultViewResponse (Prelude.Maybe Prelude.Text)
associateDefaultViewResponse_viewArn = Lens.lens (\AssociateDefaultViewResponse' {viewArn} -> viewArn) (\s@AssociateDefaultViewResponse' {} a -> s {viewArn = a} :: AssociateDefaultViewResponse)

-- | The response's http status code.
associateDefaultViewResponse_httpStatus :: Lens.Lens' AssociateDefaultViewResponse Prelude.Int
associateDefaultViewResponse_httpStatus = Lens.lens (\AssociateDefaultViewResponse' {httpStatus} -> httpStatus) (\s@AssociateDefaultViewResponse' {} a -> s {httpStatus = a} :: AssociateDefaultViewResponse)

instance Prelude.NFData AssociateDefaultViewResponse where
  rnf AssociateDefaultViewResponse' {..} =
    Prelude.rnf viewArn
      `Prelude.seq` Prelude.rnf httpStatus

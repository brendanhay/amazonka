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
-- Module      : Amazonka.ResourceExplorer2.GetDefaultView
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the Amazon Resource Name (ARN) of the view that is the default
-- for the Amazon Web Services Region in which you call this operation. You
-- can then call GetView to retrieve the details of that view.
module Amazonka.ResourceExplorer2.GetDefaultView
  ( -- * Creating a Request
    GetDefaultView (..),
    newGetDefaultView,

    -- * Destructuring the Response
    GetDefaultViewResponse (..),
    newGetDefaultViewResponse,

    -- * Response Lenses
    getDefaultViewResponse_viewArn,
    getDefaultViewResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceExplorer2.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDefaultView' smart constructor.
data GetDefaultView = GetDefaultView'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDefaultView' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetDefaultView ::
  GetDefaultView
newGetDefaultView = GetDefaultView'

instance Core.AWSRequest GetDefaultView where
  type
    AWSResponse GetDefaultView =
      GetDefaultViewResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDefaultViewResponse'
            Prelude.<$> (x Data..?> "ViewArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDefaultView where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetDefaultView where
  rnf _ = ()

instance Data.ToHeaders GetDefaultView where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDefaultView where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetDefaultView where
  toPath = Prelude.const "/GetDefaultView"

instance Data.ToQuery GetDefaultView where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDefaultViewResponse' smart constructor.
data GetDefaultViewResponse = GetDefaultViewResponse'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
    -- of the view that is the current default for the Amazon Web Services
    -- Region in which you called this operation.
    viewArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDefaultViewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'viewArn', 'getDefaultViewResponse_viewArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the view that is the current default for the Amazon Web Services
-- Region in which you called this operation.
--
-- 'httpStatus', 'getDefaultViewResponse_httpStatus' - The response's http status code.
newGetDefaultViewResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDefaultViewResponse
newGetDefaultViewResponse pHttpStatus_ =
  GetDefaultViewResponse'
    { viewArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the view that is the current default for the Amazon Web Services
-- Region in which you called this operation.
getDefaultViewResponse_viewArn :: Lens.Lens' GetDefaultViewResponse (Prelude.Maybe Prelude.Text)
getDefaultViewResponse_viewArn = Lens.lens (\GetDefaultViewResponse' {viewArn} -> viewArn) (\s@GetDefaultViewResponse' {} a -> s {viewArn = a} :: GetDefaultViewResponse)

-- | The response's http status code.
getDefaultViewResponse_httpStatus :: Lens.Lens' GetDefaultViewResponse Prelude.Int
getDefaultViewResponse_httpStatus = Lens.lens (\GetDefaultViewResponse' {httpStatus} -> httpStatus) (\s@GetDefaultViewResponse' {} a -> s {httpStatus = a} :: GetDefaultViewResponse)

instance Prelude.NFData GetDefaultViewResponse where
  rnf GetDefaultViewResponse' {..} =
    Prelude.rnf viewArn
      `Prelude.seq` Prelude.rnf httpStatus

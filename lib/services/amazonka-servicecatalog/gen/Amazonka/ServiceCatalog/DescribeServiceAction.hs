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
-- Module      : Amazonka.ServiceCatalog.DescribeServiceAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a self-service action.
module Amazonka.ServiceCatalog.DescribeServiceAction
  ( -- * Creating a Request
    DescribeServiceAction (..),
    newDescribeServiceAction,

    -- * Request Lenses
    describeServiceAction_acceptLanguage,
    describeServiceAction_id,

    -- * Destructuring the Response
    DescribeServiceActionResponse (..),
    newDescribeServiceActionResponse,

    -- * Response Lenses
    describeServiceActionResponse_serviceActionDetail,
    describeServiceActionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newDescribeServiceAction' smart constructor.
data DescribeServiceAction = DescribeServiceAction'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The self-service action identifier.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeServiceAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'describeServiceAction_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'id', 'describeServiceAction_id' - The self-service action identifier.
newDescribeServiceAction ::
  -- | 'id'
  Prelude.Text ->
  DescribeServiceAction
newDescribeServiceAction pId_ =
  DescribeServiceAction'
    { acceptLanguage =
        Prelude.Nothing,
      id = pId_
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describeServiceAction_acceptLanguage :: Lens.Lens' DescribeServiceAction (Prelude.Maybe Prelude.Text)
describeServiceAction_acceptLanguage = Lens.lens (\DescribeServiceAction' {acceptLanguage} -> acceptLanguage) (\s@DescribeServiceAction' {} a -> s {acceptLanguage = a} :: DescribeServiceAction)

-- | The self-service action identifier.
describeServiceAction_id :: Lens.Lens' DescribeServiceAction Prelude.Text
describeServiceAction_id = Lens.lens (\DescribeServiceAction' {id} -> id) (\s@DescribeServiceAction' {} a -> s {id = a} :: DescribeServiceAction)

instance Core.AWSRequest DescribeServiceAction where
  type
    AWSResponse DescribeServiceAction =
      DescribeServiceActionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeServiceActionResponse'
            Prelude.<$> (x Data..?> "ServiceActionDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeServiceAction where
  hashWithSalt _salt DescribeServiceAction' {..} =
    _salt
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` id

instance Prelude.NFData DescribeServiceAction where
  rnf DescribeServiceAction' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders DescribeServiceAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.DescribeServiceAction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeServiceAction where
  toJSON DescribeServiceAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance Data.ToPath DescribeServiceAction where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeServiceAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeServiceActionResponse' smart constructor.
data DescribeServiceActionResponse = DescribeServiceActionResponse'
  { -- | Detailed information about the self-service action.
    serviceActionDetail :: Prelude.Maybe ServiceActionDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeServiceActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceActionDetail', 'describeServiceActionResponse_serviceActionDetail' - Detailed information about the self-service action.
--
-- 'httpStatus', 'describeServiceActionResponse_httpStatus' - The response's http status code.
newDescribeServiceActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeServiceActionResponse
newDescribeServiceActionResponse pHttpStatus_ =
  DescribeServiceActionResponse'
    { serviceActionDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Detailed information about the self-service action.
describeServiceActionResponse_serviceActionDetail :: Lens.Lens' DescribeServiceActionResponse (Prelude.Maybe ServiceActionDetail)
describeServiceActionResponse_serviceActionDetail = Lens.lens (\DescribeServiceActionResponse' {serviceActionDetail} -> serviceActionDetail) (\s@DescribeServiceActionResponse' {} a -> s {serviceActionDetail = a} :: DescribeServiceActionResponse)

-- | The response's http status code.
describeServiceActionResponse_httpStatus :: Lens.Lens' DescribeServiceActionResponse Prelude.Int
describeServiceActionResponse_httpStatus = Lens.lens (\DescribeServiceActionResponse' {httpStatus} -> httpStatus) (\s@DescribeServiceActionResponse' {} a -> s {httpStatus = a} :: DescribeServiceActionResponse)

instance Prelude.NFData DescribeServiceActionResponse where
  rnf DescribeServiceActionResponse' {..} =
    Prelude.rnf serviceActionDetail
      `Prelude.seq` Prelude.rnf httpStatus

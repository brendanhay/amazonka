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
-- Module      : Amazonka.ServiceCatalog.DescribeConstraint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified constraint.
module Amazonka.ServiceCatalog.DescribeConstraint
  ( -- * Creating a Request
    DescribeConstraint (..),
    newDescribeConstraint,

    -- * Request Lenses
    describeConstraint_acceptLanguage,
    describeConstraint_id,

    -- * Destructuring the Response
    DescribeConstraintResponse (..),
    newDescribeConstraintResponse,

    -- * Response Lenses
    describeConstraintResponse_constraintDetail,
    describeConstraintResponse_status,
    describeConstraintResponse_constraintParameters,
    describeConstraintResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newDescribeConstraint' smart constructor.
data DescribeConstraint = DescribeConstraint'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the constraint.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConstraint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'describeConstraint_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'id', 'describeConstraint_id' - The identifier of the constraint.
newDescribeConstraint ::
  -- | 'id'
  Prelude.Text ->
  DescribeConstraint
newDescribeConstraint pId_ =
  DescribeConstraint'
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
describeConstraint_acceptLanguage :: Lens.Lens' DescribeConstraint (Prelude.Maybe Prelude.Text)
describeConstraint_acceptLanguage = Lens.lens (\DescribeConstraint' {acceptLanguage} -> acceptLanguage) (\s@DescribeConstraint' {} a -> s {acceptLanguage = a} :: DescribeConstraint)

-- | The identifier of the constraint.
describeConstraint_id :: Lens.Lens' DescribeConstraint Prelude.Text
describeConstraint_id = Lens.lens (\DescribeConstraint' {id} -> id) (\s@DescribeConstraint' {} a -> s {id = a} :: DescribeConstraint)

instance Core.AWSRequest DescribeConstraint where
  type
    AWSResponse DescribeConstraint =
      DescribeConstraintResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConstraintResponse'
            Prelude.<$> (x Data..?> "ConstraintDetail")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "ConstraintParameters")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeConstraint where
  hashWithSalt _salt DescribeConstraint' {..} =
    _salt `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` id

instance Prelude.NFData DescribeConstraint where
  rnf DescribeConstraint' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders DescribeConstraint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.DescribeConstraint" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeConstraint where
  toJSON DescribeConstraint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance Data.ToPath DescribeConstraint where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeConstraint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConstraintResponse' smart constructor.
data DescribeConstraintResponse = DescribeConstraintResponse'
  { -- | Information about the constraint.
    constraintDetail :: Prelude.Maybe ConstraintDetail,
    -- | The status of the current request.
    status :: Prelude.Maybe RequestStatus,
    -- | The constraint parameters.
    constraintParameters :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConstraintResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constraintDetail', 'describeConstraintResponse_constraintDetail' - Information about the constraint.
--
-- 'status', 'describeConstraintResponse_status' - The status of the current request.
--
-- 'constraintParameters', 'describeConstraintResponse_constraintParameters' - The constraint parameters.
--
-- 'httpStatus', 'describeConstraintResponse_httpStatus' - The response's http status code.
newDescribeConstraintResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConstraintResponse
newDescribeConstraintResponse pHttpStatus_ =
  DescribeConstraintResponse'
    { constraintDetail =
        Prelude.Nothing,
      status = Prelude.Nothing,
      constraintParameters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the constraint.
describeConstraintResponse_constraintDetail :: Lens.Lens' DescribeConstraintResponse (Prelude.Maybe ConstraintDetail)
describeConstraintResponse_constraintDetail = Lens.lens (\DescribeConstraintResponse' {constraintDetail} -> constraintDetail) (\s@DescribeConstraintResponse' {} a -> s {constraintDetail = a} :: DescribeConstraintResponse)

-- | The status of the current request.
describeConstraintResponse_status :: Lens.Lens' DescribeConstraintResponse (Prelude.Maybe RequestStatus)
describeConstraintResponse_status = Lens.lens (\DescribeConstraintResponse' {status} -> status) (\s@DescribeConstraintResponse' {} a -> s {status = a} :: DescribeConstraintResponse)

-- | The constraint parameters.
describeConstraintResponse_constraintParameters :: Lens.Lens' DescribeConstraintResponse (Prelude.Maybe Prelude.Text)
describeConstraintResponse_constraintParameters = Lens.lens (\DescribeConstraintResponse' {constraintParameters} -> constraintParameters) (\s@DescribeConstraintResponse' {} a -> s {constraintParameters = a} :: DescribeConstraintResponse)

-- | The response's http status code.
describeConstraintResponse_httpStatus :: Lens.Lens' DescribeConstraintResponse Prelude.Int
describeConstraintResponse_httpStatus = Lens.lens (\DescribeConstraintResponse' {httpStatus} -> httpStatus) (\s@DescribeConstraintResponse' {} a -> s {httpStatus = a} :: DescribeConstraintResponse)

instance Prelude.NFData DescribeConstraintResponse where
  rnf DescribeConstraintResponse' {..} =
    Prelude.rnf constraintDetail
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf constraintParameters
      `Prelude.seq` Prelude.rnf httpStatus

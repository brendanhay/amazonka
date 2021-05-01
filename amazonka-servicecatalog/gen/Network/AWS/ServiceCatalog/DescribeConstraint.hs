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
-- Module      : Network.AWS.ServiceCatalog.DescribeConstraint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified constraint.
module Network.AWS.ServiceCatalog.DescribeConstraint
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
    describeConstraintResponse_constraintParameters,
    describeConstraintResponse_status,
    describeConstraintResponse_constraintDetail,
    describeConstraintResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DescribeConstraint where
  type
    Rs DescribeConstraint =
      DescribeConstraintResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConstraintResponse'
            Prelude.<$> (x Prelude..?> "ConstraintParameters")
            Prelude.<*> (x Prelude..?> "Status")
            Prelude.<*> (x Prelude..?> "ConstraintDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeConstraint

instance Prelude.NFData DescribeConstraint

instance Prelude.ToHeaders DescribeConstraint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWS242ServiceCatalogService.DescribeConstraint" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeConstraint where
  toJSON DescribeConstraint' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Prelude..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("Id" Prelude..= id)
          ]
      )

instance Prelude.ToPath DescribeConstraint where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeConstraint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConstraintResponse' smart constructor.
data DescribeConstraintResponse = DescribeConstraintResponse'
  { -- | The constraint parameters.
    constraintParameters :: Prelude.Maybe Prelude.Text,
    -- | The status of the current request.
    status :: Prelude.Maybe RequestStatus,
    -- | Information about the constraint.
    constraintDetail :: Prelude.Maybe ConstraintDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeConstraintResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constraintParameters', 'describeConstraintResponse_constraintParameters' - The constraint parameters.
--
-- 'status', 'describeConstraintResponse_status' - The status of the current request.
--
-- 'constraintDetail', 'describeConstraintResponse_constraintDetail' - Information about the constraint.
--
-- 'httpStatus', 'describeConstraintResponse_httpStatus' - The response's http status code.
newDescribeConstraintResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConstraintResponse
newDescribeConstraintResponse pHttpStatus_ =
  DescribeConstraintResponse'
    { constraintParameters =
        Prelude.Nothing,
      status = Prelude.Nothing,
      constraintDetail = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The constraint parameters.
describeConstraintResponse_constraintParameters :: Lens.Lens' DescribeConstraintResponse (Prelude.Maybe Prelude.Text)
describeConstraintResponse_constraintParameters = Lens.lens (\DescribeConstraintResponse' {constraintParameters} -> constraintParameters) (\s@DescribeConstraintResponse' {} a -> s {constraintParameters = a} :: DescribeConstraintResponse)

-- | The status of the current request.
describeConstraintResponse_status :: Lens.Lens' DescribeConstraintResponse (Prelude.Maybe RequestStatus)
describeConstraintResponse_status = Lens.lens (\DescribeConstraintResponse' {status} -> status) (\s@DescribeConstraintResponse' {} a -> s {status = a} :: DescribeConstraintResponse)

-- | Information about the constraint.
describeConstraintResponse_constraintDetail :: Lens.Lens' DescribeConstraintResponse (Prelude.Maybe ConstraintDetail)
describeConstraintResponse_constraintDetail = Lens.lens (\DescribeConstraintResponse' {constraintDetail} -> constraintDetail) (\s@DescribeConstraintResponse' {} a -> s {constraintDetail = a} :: DescribeConstraintResponse)

-- | The response's http status code.
describeConstraintResponse_httpStatus :: Lens.Lens' DescribeConstraintResponse Prelude.Int
describeConstraintResponse_httpStatus = Lens.lens (\DescribeConstraintResponse' {httpStatus} -> httpStatus) (\s@DescribeConstraintResponse' {} a -> s {httpStatus = a} :: DescribeConstraintResponse)

instance Prelude.NFData DescribeConstraintResponse

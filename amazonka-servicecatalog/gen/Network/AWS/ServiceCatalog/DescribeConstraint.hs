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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
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
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The identifier of the constraint.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeConstraint
newDescribeConstraint pId_ =
  DescribeConstraint'
    { acceptLanguage = Core.Nothing,
      id = pId_
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describeConstraint_acceptLanguage :: Lens.Lens' DescribeConstraint (Core.Maybe Core.Text)
describeConstraint_acceptLanguage = Lens.lens (\DescribeConstraint' {acceptLanguage} -> acceptLanguage) (\s@DescribeConstraint' {} a -> s {acceptLanguage = a} :: DescribeConstraint)

-- | The identifier of the constraint.
describeConstraint_id :: Lens.Lens' DescribeConstraint Core.Text
describeConstraint_id = Lens.lens (\DescribeConstraint' {id} -> id) (\s@DescribeConstraint' {} a -> s {id = a} :: DescribeConstraint)

instance Core.AWSRequest DescribeConstraint where
  type
    AWSResponse DescribeConstraint =
      DescribeConstraintResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConstraintResponse'
            Core.<$> (x Core..?> "ConstraintParameters")
            Core.<*> (x Core..?> "Status")
            Core.<*> (x Core..?> "ConstraintDetail")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeConstraint

instance Core.NFData DescribeConstraint

instance Core.ToHeaders DescribeConstraint where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DescribeConstraint" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeConstraint where
  toJSON DescribeConstraint' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("Id" Core..= id)
          ]
      )

instance Core.ToPath DescribeConstraint where
  toPath = Core.const "/"

instance Core.ToQuery DescribeConstraint where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeConstraintResponse' smart constructor.
data DescribeConstraintResponse = DescribeConstraintResponse'
  { -- | The constraint parameters.
    constraintParameters :: Core.Maybe Core.Text,
    -- | The status of the current request.
    status :: Core.Maybe RequestStatus,
    -- | Information about the constraint.
    constraintDetail :: Core.Maybe ConstraintDetail,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeConstraintResponse
newDescribeConstraintResponse pHttpStatus_ =
  DescribeConstraintResponse'
    { constraintParameters =
        Core.Nothing,
      status = Core.Nothing,
      constraintDetail = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The constraint parameters.
describeConstraintResponse_constraintParameters :: Lens.Lens' DescribeConstraintResponse (Core.Maybe Core.Text)
describeConstraintResponse_constraintParameters = Lens.lens (\DescribeConstraintResponse' {constraintParameters} -> constraintParameters) (\s@DescribeConstraintResponse' {} a -> s {constraintParameters = a} :: DescribeConstraintResponse)

-- | The status of the current request.
describeConstraintResponse_status :: Lens.Lens' DescribeConstraintResponse (Core.Maybe RequestStatus)
describeConstraintResponse_status = Lens.lens (\DescribeConstraintResponse' {status} -> status) (\s@DescribeConstraintResponse' {} a -> s {status = a} :: DescribeConstraintResponse)

-- | Information about the constraint.
describeConstraintResponse_constraintDetail :: Lens.Lens' DescribeConstraintResponse (Core.Maybe ConstraintDetail)
describeConstraintResponse_constraintDetail = Lens.lens (\DescribeConstraintResponse' {constraintDetail} -> constraintDetail) (\s@DescribeConstraintResponse' {} a -> s {constraintDetail = a} :: DescribeConstraintResponse)

-- | The response's http status code.
describeConstraintResponse_httpStatus :: Lens.Lens' DescribeConstraintResponse Core.Int
describeConstraintResponse_httpStatus = Lens.lens (\DescribeConstraintResponse' {httpStatus} -> httpStatus) (\s@DescribeConstraintResponse' {} a -> s {httpStatus = a} :: DescribeConstraintResponse)

instance Core.NFData DescribeConstraintResponse

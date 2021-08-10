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
-- Module      : Network.AWS.Inspector.DescribeCrossAccountAccessRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the IAM role that enables Amazon Inspector to access your AWS
-- account.
module Network.AWS.Inspector.DescribeCrossAccountAccessRole
  ( -- * Creating a Request
    DescribeCrossAccountAccessRole (..),
    newDescribeCrossAccountAccessRole,

    -- * Destructuring the Response
    DescribeCrossAccountAccessRoleResponse (..),
    newDescribeCrossAccountAccessRoleResponse,

    -- * Response Lenses
    describeCrossAccountAccessRoleResponse_httpStatus,
    describeCrossAccountAccessRoleResponse_roleArn,
    describeCrossAccountAccessRoleResponse_valid,
    describeCrossAccountAccessRoleResponse_registeredAt,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeCrossAccountAccessRole' smart constructor.
data DescribeCrossAccountAccessRole = DescribeCrossAccountAccessRole'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCrossAccountAccessRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeCrossAccountAccessRole ::
  DescribeCrossAccountAccessRole
newDescribeCrossAccountAccessRole =
  DescribeCrossAccountAccessRole'

instance
  Core.AWSRequest
    DescribeCrossAccountAccessRole
  where
  type
    AWSResponse DescribeCrossAccountAccessRole =
      DescribeCrossAccountAccessRoleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCrossAccountAccessRoleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "roleArn")
            Prelude.<*> (x Core..:> "valid")
            Prelude.<*> (x Core..:> "registeredAt")
      )

instance
  Prelude.Hashable
    DescribeCrossAccountAccessRole

instance
  Prelude.NFData
    DescribeCrossAccountAccessRole

instance
  Core.ToHeaders
    DescribeCrossAccountAccessRole
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.DescribeCrossAccountAccessRole" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeCrossAccountAccessRole where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath DescribeCrossAccountAccessRole where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeCrossAccountAccessRole where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCrossAccountAccessRoleResponse' smart constructor.
data DescribeCrossAccountAccessRoleResponse = DescribeCrossAccountAccessRoleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN that specifies the IAM role that Amazon Inspector uses to access
    -- your AWS account.
    roleArn :: Prelude.Text,
    -- | A Boolean value that specifies whether the IAM role has the necessary
    -- policies attached to enable Amazon Inspector to access your AWS account.
    valid :: Prelude.Bool,
    -- | The date when the cross-account access role was registered.
    registeredAt :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCrossAccountAccessRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeCrossAccountAccessRoleResponse_httpStatus' - The response's http status code.
--
-- 'roleArn', 'describeCrossAccountAccessRoleResponse_roleArn' - The ARN that specifies the IAM role that Amazon Inspector uses to access
-- your AWS account.
--
-- 'valid', 'describeCrossAccountAccessRoleResponse_valid' - A Boolean value that specifies whether the IAM role has the necessary
-- policies attached to enable Amazon Inspector to access your AWS account.
--
-- 'registeredAt', 'describeCrossAccountAccessRoleResponse_registeredAt' - The date when the cross-account access role was registered.
newDescribeCrossAccountAccessRoleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'valid'
  Prelude.Bool ->
  -- | 'registeredAt'
  Prelude.UTCTime ->
  DescribeCrossAccountAccessRoleResponse
newDescribeCrossAccountAccessRoleResponse
  pHttpStatus_
  pRoleArn_
  pValid_
  pRegisteredAt_ =
    DescribeCrossAccountAccessRoleResponse'
      { httpStatus =
          pHttpStatus_,
        roleArn = pRoleArn_,
        valid = pValid_,
        registeredAt =
          Core._Time Lens.# pRegisteredAt_
      }

-- | The response's http status code.
describeCrossAccountAccessRoleResponse_httpStatus :: Lens.Lens' DescribeCrossAccountAccessRoleResponse Prelude.Int
describeCrossAccountAccessRoleResponse_httpStatus = Lens.lens (\DescribeCrossAccountAccessRoleResponse' {httpStatus} -> httpStatus) (\s@DescribeCrossAccountAccessRoleResponse' {} a -> s {httpStatus = a} :: DescribeCrossAccountAccessRoleResponse)

-- | The ARN that specifies the IAM role that Amazon Inspector uses to access
-- your AWS account.
describeCrossAccountAccessRoleResponse_roleArn :: Lens.Lens' DescribeCrossAccountAccessRoleResponse Prelude.Text
describeCrossAccountAccessRoleResponse_roleArn = Lens.lens (\DescribeCrossAccountAccessRoleResponse' {roleArn} -> roleArn) (\s@DescribeCrossAccountAccessRoleResponse' {} a -> s {roleArn = a} :: DescribeCrossAccountAccessRoleResponse)

-- | A Boolean value that specifies whether the IAM role has the necessary
-- policies attached to enable Amazon Inspector to access your AWS account.
describeCrossAccountAccessRoleResponse_valid :: Lens.Lens' DescribeCrossAccountAccessRoleResponse Prelude.Bool
describeCrossAccountAccessRoleResponse_valid = Lens.lens (\DescribeCrossAccountAccessRoleResponse' {valid} -> valid) (\s@DescribeCrossAccountAccessRoleResponse' {} a -> s {valid = a} :: DescribeCrossAccountAccessRoleResponse)

-- | The date when the cross-account access role was registered.
describeCrossAccountAccessRoleResponse_registeredAt :: Lens.Lens' DescribeCrossAccountAccessRoleResponse Prelude.UTCTime
describeCrossAccountAccessRoleResponse_registeredAt = Lens.lens (\DescribeCrossAccountAccessRoleResponse' {registeredAt} -> registeredAt) (\s@DescribeCrossAccountAccessRoleResponse' {} a -> s {registeredAt = a} :: DescribeCrossAccountAccessRoleResponse) Prelude.. Core._Time

instance
  Prelude.NFData
    DescribeCrossAccountAccessRoleResponse

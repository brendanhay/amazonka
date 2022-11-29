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
-- Module      : Amazonka.QuickSight.DescribeTemplatePermissions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes read and write permissions on a template.
module Amazonka.QuickSight.DescribeTemplatePermissions
  ( -- * Creating a Request
    DescribeTemplatePermissions (..),
    newDescribeTemplatePermissions,

    -- * Request Lenses
    describeTemplatePermissions_awsAccountId,
    describeTemplatePermissions_templateId,

    -- * Destructuring the Response
    DescribeTemplatePermissionsResponse (..),
    newDescribeTemplatePermissionsResponse,

    -- * Response Lenses
    describeTemplatePermissionsResponse_requestId,
    describeTemplatePermissionsResponse_permissions,
    describeTemplatePermissionsResponse_templateId,
    describeTemplatePermissionsResponse_templateArn,
    describeTemplatePermissionsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTemplatePermissions' smart constructor.
data DescribeTemplatePermissions = DescribeTemplatePermissions'
  { -- | The ID of the Amazon Web Services account that contains the template
    -- that you\'re describing.
    awsAccountId :: Prelude.Text,
    -- | The ID for the template.
    templateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTemplatePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeTemplatePermissions_awsAccountId' - The ID of the Amazon Web Services account that contains the template
-- that you\'re describing.
--
-- 'templateId', 'describeTemplatePermissions_templateId' - The ID for the template.
newDescribeTemplatePermissions ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'templateId'
  Prelude.Text ->
  DescribeTemplatePermissions
newDescribeTemplatePermissions
  pAwsAccountId_
  pTemplateId_ =
    DescribeTemplatePermissions'
      { awsAccountId =
          pAwsAccountId_,
        templateId = pTemplateId_
      }

-- | The ID of the Amazon Web Services account that contains the template
-- that you\'re describing.
describeTemplatePermissions_awsAccountId :: Lens.Lens' DescribeTemplatePermissions Prelude.Text
describeTemplatePermissions_awsAccountId = Lens.lens (\DescribeTemplatePermissions' {awsAccountId} -> awsAccountId) (\s@DescribeTemplatePermissions' {} a -> s {awsAccountId = a} :: DescribeTemplatePermissions)

-- | The ID for the template.
describeTemplatePermissions_templateId :: Lens.Lens' DescribeTemplatePermissions Prelude.Text
describeTemplatePermissions_templateId = Lens.lens (\DescribeTemplatePermissions' {templateId} -> templateId) (\s@DescribeTemplatePermissions' {} a -> s {templateId = a} :: DescribeTemplatePermissions)

instance Core.AWSRequest DescribeTemplatePermissions where
  type
    AWSResponse DescribeTemplatePermissions =
      DescribeTemplatePermissionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTemplatePermissionsResponse'
            Prelude.<$> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "Permissions")
            Prelude.<*> (x Core..?> "TemplateId")
            Prelude.<*> (x Core..?> "TemplateArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTemplatePermissions where
  hashWithSalt _salt DescribeTemplatePermissions' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` templateId

instance Prelude.NFData DescribeTemplatePermissions where
  rnf DescribeTemplatePermissions' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf templateId

instance Core.ToHeaders DescribeTemplatePermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeTemplatePermissions where
  toPath DescribeTemplatePermissions' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/templates/",
        Core.toBS templateId,
        "/permissions"
      ]

instance Core.ToQuery DescribeTemplatePermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTemplatePermissionsResponse' smart constructor.
data DescribeTemplatePermissionsResponse = DescribeTemplatePermissionsResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | A list of resource permissions to be set on the template.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The ID for the template.
    templateId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the template.
    templateArn :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTemplatePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'describeTemplatePermissionsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'permissions', 'describeTemplatePermissionsResponse_permissions' - A list of resource permissions to be set on the template.
--
-- 'templateId', 'describeTemplatePermissionsResponse_templateId' - The ID for the template.
--
-- 'templateArn', 'describeTemplatePermissionsResponse_templateArn' - The Amazon Resource Name (ARN) of the template.
--
-- 'status', 'describeTemplatePermissionsResponse_status' - The HTTP status of the request.
newDescribeTemplatePermissionsResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeTemplatePermissionsResponse
newDescribeTemplatePermissionsResponse pStatus_ =
  DescribeTemplatePermissionsResponse'
    { requestId =
        Prelude.Nothing,
      permissions = Prelude.Nothing,
      templateId = Prelude.Nothing,
      templateArn = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
describeTemplatePermissionsResponse_requestId :: Lens.Lens' DescribeTemplatePermissionsResponse (Prelude.Maybe Prelude.Text)
describeTemplatePermissionsResponse_requestId = Lens.lens (\DescribeTemplatePermissionsResponse' {requestId} -> requestId) (\s@DescribeTemplatePermissionsResponse' {} a -> s {requestId = a} :: DescribeTemplatePermissionsResponse)

-- | A list of resource permissions to be set on the template.
describeTemplatePermissionsResponse_permissions :: Lens.Lens' DescribeTemplatePermissionsResponse (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
describeTemplatePermissionsResponse_permissions = Lens.lens (\DescribeTemplatePermissionsResponse' {permissions} -> permissions) (\s@DescribeTemplatePermissionsResponse' {} a -> s {permissions = a} :: DescribeTemplatePermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID for the template.
describeTemplatePermissionsResponse_templateId :: Lens.Lens' DescribeTemplatePermissionsResponse (Prelude.Maybe Prelude.Text)
describeTemplatePermissionsResponse_templateId = Lens.lens (\DescribeTemplatePermissionsResponse' {templateId} -> templateId) (\s@DescribeTemplatePermissionsResponse' {} a -> s {templateId = a} :: DescribeTemplatePermissionsResponse)

-- | The Amazon Resource Name (ARN) of the template.
describeTemplatePermissionsResponse_templateArn :: Lens.Lens' DescribeTemplatePermissionsResponse (Prelude.Maybe Prelude.Text)
describeTemplatePermissionsResponse_templateArn = Lens.lens (\DescribeTemplatePermissionsResponse' {templateArn} -> templateArn) (\s@DescribeTemplatePermissionsResponse' {} a -> s {templateArn = a} :: DescribeTemplatePermissionsResponse)

-- | The HTTP status of the request.
describeTemplatePermissionsResponse_status :: Lens.Lens' DescribeTemplatePermissionsResponse Prelude.Int
describeTemplatePermissionsResponse_status = Lens.lens (\DescribeTemplatePermissionsResponse' {status} -> status) (\s@DescribeTemplatePermissionsResponse' {} a -> s {status = a} :: DescribeTemplatePermissionsResponse)

instance
  Prelude.NFData
    DescribeTemplatePermissionsResponse
  where
  rnf DescribeTemplatePermissionsResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf templateId
      `Prelude.seq` Prelude.rnf templateArn
      `Prelude.seq` Prelude.rnf status

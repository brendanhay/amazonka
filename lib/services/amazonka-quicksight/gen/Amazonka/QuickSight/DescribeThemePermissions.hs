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
-- Module      : Amazonka.QuickSight.DescribeThemePermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the read and write permissions for a theme.
module Amazonka.QuickSight.DescribeThemePermissions
  ( -- * Creating a Request
    DescribeThemePermissions (..),
    newDescribeThemePermissions,

    -- * Request Lenses
    describeThemePermissions_awsAccountId,
    describeThemePermissions_themeId,

    -- * Destructuring the Response
    DescribeThemePermissionsResponse (..),
    newDescribeThemePermissionsResponse,

    -- * Response Lenses
    describeThemePermissionsResponse_permissions,
    describeThemePermissionsResponse_requestId,
    describeThemePermissionsResponse_themeArn,
    describeThemePermissionsResponse_themeId,
    describeThemePermissionsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeThemePermissions' smart constructor.
data DescribeThemePermissions = DescribeThemePermissions'
  { -- | The ID of the Amazon Web Services account that contains the theme that
    -- you\'re describing.
    awsAccountId :: Prelude.Text,
    -- | The ID for the theme that you want to describe permissions for.
    themeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeThemePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeThemePermissions_awsAccountId' - The ID of the Amazon Web Services account that contains the theme that
-- you\'re describing.
--
-- 'themeId', 'describeThemePermissions_themeId' - The ID for the theme that you want to describe permissions for.
newDescribeThemePermissions ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'themeId'
  Prelude.Text ->
  DescribeThemePermissions
newDescribeThemePermissions pAwsAccountId_ pThemeId_ =
  DescribeThemePermissions'
    { awsAccountId =
        pAwsAccountId_,
      themeId = pThemeId_
    }

-- | The ID of the Amazon Web Services account that contains the theme that
-- you\'re describing.
describeThemePermissions_awsAccountId :: Lens.Lens' DescribeThemePermissions Prelude.Text
describeThemePermissions_awsAccountId = Lens.lens (\DescribeThemePermissions' {awsAccountId} -> awsAccountId) (\s@DescribeThemePermissions' {} a -> s {awsAccountId = a} :: DescribeThemePermissions)

-- | The ID for the theme that you want to describe permissions for.
describeThemePermissions_themeId :: Lens.Lens' DescribeThemePermissions Prelude.Text
describeThemePermissions_themeId = Lens.lens (\DescribeThemePermissions' {themeId} -> themeId) (\s@DescribeThemePermissions' {} a -> s {themeId = a} :: DescribeThemePermissions)

instance Core.AWSRequest DescribeThemePermissions where
  type
    AWSResponse DescribeThemePermissions =
      DescribeThemePermissionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeThemePermissionsResponse'
            Prelude.<$> (x Data..?> "Permissions")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "ThemeArn")
            Prelude.<*> (x Data..?> "ThemeId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeThemePermissions where
  hashWithSalt _salt DescribeThemePermissions' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` themeId

instance Prelude.NFData DescribeThemePermissions where
  rnf DescribeThemePermissions' {..} =
    Prelude.rnf awsAccountId `Prelude.seq`
      Prelude.rnf themeId

instance Data.ToHeaders DescribeThemePermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeThemePermissions where
  toPath DescribeThemePermissions' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/themes/",
        Data.toBS themeId,
        "/permissions"
      ]

instance Data.ToQuery DescribeThemePermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeThemePermissionsResponse' smart constructor.
data DescribeThemePermissionsResponse = DescribeThemePermissionsResponse'
  { -- | A list of resource permissions set on the theme.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the theme.
    themeArn :: Prelude.Maybe Prelude.Text,
    -- | The ID for the theme.
    themeId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeThemePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissions', 'describeThemePermissionsResponse_permissions' - A list of resource permissions set on the theme.
--
-- 'requestId', 'describeThemePermissionsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'themeArn', 'describeThemePermissionsResponse_themeArn' - The Amazon Resource Name (ARN) of the theme.
--
-- 'themeId', 'describeThemePermissionsResponse_themeId' - The ID for the theme.
--
-- 'status', 'describeThemePermissionsResponse_status' - The HTTP status of the request.
newDescribeThemePermissionsResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeThemePermissionsResponse
newDescribeThemePermissionsResponse pStatus_ =
  DescribeThemePermissionsResponse'
    { permissions =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      themeArn = Prelude.Nothing,
      themeId = Prelude.Nothing,
      status = pStatus_
    }

-- | A list of resource permissions set on the theme.
describeThemePermissionsResponse_permissions :: Lens.Lens' DescribeThemePermissionsResponse (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
describeThemePermissionsResponse_permissions = Lens.lens (\DescribeThemePermissionsResponse' {permissions} -> permissions) (\s@DescribeThemePermissionsResponse' {} a -> s {permissions = a} :: DescribeThemePermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services request ID for this operation.
describeThemePermissionsResponse_requestId :: Lens.Lens' DescribeThemePermissionsResponse (Prelude.Maybe Prelude.Text)
describeThemePermissionsResponse_requestId = Lens.lens (\DescribeThemePermissionsResponse' {requestId} -> requestId) (\s@DescribeThemePermissionsResponse' {} a -> s {requestId = a} :: DescribeThemePermissionsResponse)

-- | The Amazon Resource Name (ARN) of the theme.
describeThemePermissionsResponse_themeArn :: Lens.Lens' DescribeThemePermissionsResponse (Prelude.Maybe Prelude.Text)
describeThemePermissionsResponse_themeArn = Lens.lens (\DescribeThemePermissionsResponse' {themeArn} -> themeArn) (\s@DescribeThemePermissionsResponse' {} a -> s {themeArn = a} :: DescribeThemePermissionsResponse)

-- | The ID for the theme.
describeThemePermissionsResponse_themeId :: Lens.Lens' DescribeThemePermissionsResponse (Prelude.Maybe Prelude.Text)
describeThemePermissionsResponse_themeId = Lens.lens (\DescribeThemePermissionsResponse' {themeId} -> themeId) (\s@DescribeThemePermissionsResponse' {} a -> s {themeId = a} :: DescribeThemePermissionsResponse)

-- | The HTTP status of the request.
describeThemePermissionsResponse_status :: Lens.Lens' DescribeThemePermissionsResponse Prelude.Int
describeThemePermissionsResponse_status = Lens.lens (\DescribeThemePermissionsResponse' {status} -> status) (\s@DescribeThemePermissionsResponse' {} a -> s {status = a} :: DescribeThemePermissionsResponse)

instance
  Prelude.NFData
    DescribeThemePermissionsResponse
  where
  rnf DescribeThemePermissionsResponse' {..} =
    Prelude.rnf permissions `Prelude.seq`
      Prelude.rnf requestId `Prelude.seq`
        Prelude.rnf themeArn `Prelude.seq`
          Prelude.rnf themeId `Prelude.seq`
            Prelude.rnf status

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
-- Module      : Amazonka.ApplicationInsights.DescribeApplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the application.
module Amazonka.ApplicationInsights.DescribeApplication
  ( -- * Creating a Request
    DescribeApplication (..),
    newDescribeApplication,

    -- * Request Lenses
    describeApplication_resourceGroupName,

    -- * Destructuring the Response
    DescribeApplicationResponse (..),
    newDescribeApplicationResponse,

    -- * Response Lenses
    describeApplicationResponse_applicationInfo,
    describeApplicationResponse_httpStatus,
  )
where

import Amazonka.ApplicationInsights.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeApplication' smart constructor.
data DescribeApplication = DescribeApplication'
  { -- | The name of the resource group.
    resourceGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceGroupName', 'describeApplication_resourceGroupName' - The name of the resource group.
newDescribeApplication ::
  -- | 'resourceGroupName'
  Prelude.Text ->
  DescribeApplication
newDescribeApplication pResourceGroupName_ =
  DescribeApplication'
    { resourceGroupName =
        pResourceGroupName_
    }

-- | The name of the resource group.
describeApplication_resourceGroupName :: Lens.Lens' DescribeApplication Prelude.Text
describeApplication_resourceGroupName = Lens.lens (\DescribeApplication' {resourceGroupName} -> resourceGroupName) (\s@DescribeApplication' {} a -> s {resourceGroupName = a} :: DescribeApplication)

instance Core.AWSRequest DescribeApplication where
  type
    AWSResponse DescribeApplication =
      DescribeApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeApplicationResponse'
            Prelude.<$> (x Core..?> "ApplicationInfo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeApplication where
  hashWithSalt _salt DescribeApplication' {..} =
    _salt `Prelude.hashWithSalt` resourceGroupName

instance Prelude.NFData DescribeApplication where
  rnf DescribeApplication' {..} =
    Prelude.rnf resourceGroupName

instance Core.ToHeaders DescribeApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "EC2WindowsBarleyService.DescribeApplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeApplication where
  toJSON DescribeApplication' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResourceGroupName" Core..= resourceGroupName)
          ]
      )

instance Core.ToPath DescribeApplication where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeApplicationResponse' smart constructor.
data DescribeApplicationResponse = DescribeApplicationResponse'
  { -- | Information about the application.
    applicationInfo :: Prelude.Maybe ApplicationInfo,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationInfo', 'describeApplicationResponse_applicationInfo' - Information about the application.
--
-- 'httpStatus', 'describeApplicationResponse_httpStatus' - The response's http status code.
newDescribeApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeApplicationResponse
newDescribeApplicationResponse pHttpStatus_ =
  DescribeApplicationResponse'
    { applicationInfo =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the application.
describeApplicationResponse_applicationInfo :: Lens.Lens' DescribeApplicationResponse (Prelude.Maybe ApplicationInfo)
describeApplicationResponse_applicationInfo = Lens.lens (\DescribeApplicationResponse' {applicationInfo} -> applicationInfo) (\s@DescribeApplicationResponse' {} a -> s {applicationInfo = a} :: DescribeApplicationResponse)

-- | The response's http status code.
describeApplicationResponse_httpStatus :: Lens.Lens' DescribeApplicationResponse Prelude.Int
describeApplicationResponse_httpStatus = Lens.lens (\DescribeApplicationResponse' {httpStatus} -> httpStatus) (\s@DescribeApplicationResponse' {} a -> s {httpStatus = a} :: DescribeApplicationResponse)

instance Prelude.NFData DescribeApplicationResponse where
  rnf DescribeApplicationResponse' {..} =
    Prelude.rnf applicationInfo
      `Prelude.seq` Prelude.rnf httpStatus

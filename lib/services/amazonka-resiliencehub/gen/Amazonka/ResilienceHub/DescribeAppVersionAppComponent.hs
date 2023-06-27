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
-- Module      : Amazonka.ResilienceHub.DescribeAppVersionAppComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an Application Component in the Resilience Hub application.
module Amazonka.ResilienceHub.DescribeAppVersionAppComponent
  ( -- * Creating a Request
    DescribeAppVersionAppComponent (..),
    newDescribeAppVersionAppComponent,

    -- * Request Lenses
    describeAppVersionAppComponent_appArn,
    describeAppVersionAppComponent_appVersion,
    describeAppVersionAppComponent_id,

    -- * Destructuring the Response
    DescribeAppVersionAppComponentResponse (..),
    newDescribeAppVersionAppComponentResponse,

    -- * Response Lenses
    describeAppVersionAppComponentResponse_appComponent,
    describeAppVersionAppComponentResponse_httpStatus,
    describeAppVersionAppComponentResponse_appArn,
    describeAppVersionAppComponentResponse_appVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAppVersionAppComponent' smart constructor.
data DescribeAppVersionAppComponent = DescribeAppVersionAppComponent'
  { -- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
    -- format for this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
    -- information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    appArn :: Prelude.Text,
    -- | The Resilience Hub application version.
    appVersion :: Prelude.Text,
    -- | The identifier of the Application Component.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAppVersionAppComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appArn', 'describeAppVersionAppComponent_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'appVersion', 'describeAppVersionAppComponent_appVersion' - The Resilience Hub application version.
--
-- 'id', 'describeAppVersionAppComponent_id' - The identifier of the Application Component.
newDescribeAppVersionAppComponent ::
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  DescribeAppVersionAppComponent
newDescribeAppVersionAppComponent
  pAppArn_
  pAppVersion_
  pId_ =
    DescribeAppVersionAppComponent'
      { appArn = pAppArn_,
        appVersion = pAppVersion_,
        id = pId_
      }

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
describeAppVersionAppComponent_appArn :: Lens.Lens' DescribeAppVersionAppComponent Prelude.Text
describeAppVersionAppComponent_appArn = Lens.lens (\DescribeAppVersionAppComponent' {appArn} -> appArn) (\s@DescribeAppVersionAppComponent' {} a -> s {appArn = a} :: DescribeAppVersionAppComponent)

-- | The Resilience Hub application version.
describeAppVersionAppComponent_appVersion :: Lens.Lens' DescribeAppVersionAppComponent Prelude.Text
describeAppVersionAppComponent_appVersion = Lens.lens (\DescribeAppVersionAppComponent' {appVersion} -> appVersion) (\s@DescribeAppVersionAppComponent' {} a -> s {appVersion = a} :: DescribeAppVersionAppComponent)

-- | The identifier of the Application Component.
describeAppVersionAppComponent_id :: Lens.Lens' DescribeAppVersionAppComponent Prelude.Text
describeAppVersionAppComponent_id = Lens.lens (\DescribeAppVersionAppComponent' {id} -> id) (\s@DescribeAppVersionAppComponent' {} a -> s {id = a} :: DescribeAppVersionAppComponent)

instance
  Core.AWSRequest
    DescribeAppVersionAppComponent
  where
  type
    AWSResponse DescribeAppVersionAppComponent =
      DescribeAppVersionAppComponentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAppVersionAppComponentResponse'
            Prelude.<$> (x Data..?> "appComponent")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "appArn")
            Prelude.<*> (x Data..:> "appVersion")
      )

instance
  Prelude.Hashable
    DescribeAppVersionAppComponent
  where
  hashWithSalt
    _salt
    DescribeAppVersionAppComponent' {..} =
      _salt
        `Prelude.hashWithSalt` appArn
        `Prelude.hashWithSalt` appVersion
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    DescribeAppVersionAppComponent
  where
  rnf DescribeAppVersionAppComponent' {..} =
    Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion
      `Prelude.seq` Prelude.rnf id

instance
  Data.ToHeaders
    DescribeAppVersionAppComponent
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAppVersionAppComponent where
  toJSON DescribeAppVersionAppComponent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("appArn" Data..= appArn),
            Prelude.Just ("appVersion" Data..= appVersion),
            Prelude.Just ("id" Data..= id)
          ]
      )

instance Data.ToPath DescribeAppVersionAppComponent where
  toPath =
    Prelude.const "/describe-app-version-app-component"

instance Data.ToQuery DescribeAppVersionAppComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAppVersionAppComponentResponse' smart constructor.
data DescribeAppVersionAppComponentResponse = DescribeAppVersionAppComponentResponse'
  { -- | The list of Application Components that belong to this resource.
    appComponent :: Prelude.Maybe AppComponent,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
    -- format for this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
    -- information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    appArn :: Prelude.Text,
    -- | The Resilience Hub application version.
    appVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAppVersionAppComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appComponent', 'describeAppVersionAppComponentResponse_appComponent' - The list of Application Components that belong to this resource.
--
-- 'httpStatus', 'describeAppVersionAppComponentResponse_httpStatus' - The response's http status code.
--
-- 'appArn', 'describeAppVersionAppComponentResponse_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'appVersion', 'describeAppVersionAppComponentResponse_appVersion' - The Resilience Hub application version.
newDescribeAppVersionAppComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  DescribeAppVersionAppComponentResponse
newDescribeAppVersionAppComponentResponse
  pHttpStatus_
  pAppArn_
  pAppVersion_ =
    DescribeAppVersionAppComponentResponse'
      { appComponent =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        appArn = pAppArn_,
        appVersion = pAppVersion_
      }

-- | The list of Application Components that belong to this resource.
describeAppVersionAppComponentResponse_appComponent :: Lens.Lens' DescribeAppVersionAppComponentResponse (Prelude.Maybe AppComponent)
describeAppVersionAppComponentResponse_appComponent = Lens.lens (\DescribeAppVersionAppComponentResponse' {appComponent} -> appComponent) (\s@DescribeAppVersionAppComponentResponse' {} a -> s {appComponent = a} :: DescribeAppVersionAppComponentResponse)

-- | The response's http status code.
describeAppVersionAppComponentResponse_httpStatus :: Lens.Lens' DescribeAppVersionAppComponentResponse Prelude.Int
describeAppVersionAppComponentResponse_httpStatus = Lens.lens (\DescribeAppVersionAppComponentResponse' {httpStatus} -> httpStatus) (\s@DescribeAppVersionAppComponentResponse' {} a -> s {httpStatus = a} :: DescribeAppVersionAppComponentResponse)

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
describeAppVersionAppComponentResponse_appArn :: Lens.Lens' DescribeAppVersionAppComponentResponse Prelude.Text
describeAppVersionAppComponentResponse_appArn = Lens.lens (\DescribeAppVersionAppComponentResponse' {appArn} -> appArn) (\s@DescribeAppVersionAppComponentResponse' {} a -> s {appArn = a} :: DescribeAppVersionAppComponentResponse)

-- | The Resilience Hub application version.
describeAppVersionAppComponentResponse_appVersion :: Lens.Lens' DescribeAppVersionAppComponentResponse Prelude.Text
describeAppVersionAppComponentResponse_appVersion = Lens.lens (\DescribeAppVersionAppComponentResponse' {appVersion} -> appVersion) (\s@DescribeAppVersionAppComponentResponse' {} a -> s {appVersion = a} :: DescribeAppVersionAppComponentResponse)

instance
  Prelude.NFData
    DescribeAppVersionAppComponentResponse
  where
  rnf DescribeAppVersionAppComponentResponse' {..} =
    Prelude.rnf appComponent
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion

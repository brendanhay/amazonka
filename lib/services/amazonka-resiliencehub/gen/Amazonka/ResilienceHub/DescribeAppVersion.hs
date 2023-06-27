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
-- Module      : Amazonka.ResilienceHub.DescribeAppVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Resilience Hub application version.
module Amazonka.ResilienceHub.DescribeAppVersion
  ( -- * Creating a Request
    DescribeAppVersion (..),
    newDescribeAppVersion,

    -- * Request Lenses
    describeAppVersion_appArn,
    describeAppVersion_appVersion,

    -- * Destructuring the Response
    DescribeAppVersionResponse (..),
    newDescribeAppVersionResponse,

    -- * Response Lenses
    describeAppVersionResponse_additionalInfo,
    describeAppVersionResponse_httpStatus,
    describeAppVersionResponse_appArn,
    describeAppVersionResponse_appVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAppVersion' smart constructor.
data DescribeAppVersion = DescribeAppVersion'
  { -- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
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
-- Create a value of 'DescribeAppVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appArn', 'describeAppVersion_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'appVersion', 'describeAppVersion_appVersion' - The Resilience Hub application version.
newDescribeAppVersion ::
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  DescribeAppVersion
newDescribeAppVersion pAppArn_ pAppVersion_ =
  DescribeAppVersion'
    { appArn = pAppArn_,
      appVersion = pAppVersion_
    }

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
describeAppVersion_appArn :: Lens.Lens' DescribeAppVersion Prelude.Text
describeAppVersion_appArn = Lens.lens (\DescribeAppVersion' {appArn} -> appArn) (\s@DescribeAppVersion' {} a -> s {appArn = a} :: DescribeAppVersion)

-- | The Resilience Hub application version.
describeAppVersion_appVersion :: Lens.Lens' DescribeAppVersion Prelude.Text
describeAppVersion_appVersion = Lens.lens (\DescribeAppVersion' {appVersion} -> appVersion) (\s@DescribeAppVersion' {} a -> s {appVersion = a} :: DescribeAppVersion)

instance Core.AWSRequest DescribeAppVersion where
  type
    AWSResponse DescribeAppVersion =
      DescribeAppVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAppVersionResponse'
            Prelude.<$> (x Data..?> "additionalInfo" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "appArn")
            Prelude.<*> (x Data..:> "appVersion")
      )

instance Prelude.Hashable DescribeAppVersion where
  hashWithSalt _salt DescribeAppVersion' {..} =
    _salt
      `Prelude.hashWithSalt` appArn
      `Prelude.hashWithSalt` appVersion

instance Prelude.NFData DescribeAppVersion where
  rnf DescribeAppVersion' {..} =
    Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion

instance Data.ToHeaders DescribeAppVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAppVersion where
  toJSON DescribeAppVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("appArn" Data..= appArn),
            Prelude.Just ("appVersion" Data..= appVersion)
          ]
      )

instance Data.ToPath DescribeAppVersion where
  toPath = Prelude.const "/describe-app-version"

instance Data.ToQuery DescribeAppVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAppVersionResponse' smart constructor.
data DescribeAppVersionResponse = DescribeAppVersionResponse'
  { -- | Additional configuration parameters for an Resilience Hub application.
    -- If you want to implement @additionalInfo@ through the Resilience Hub
    -- console rather than using an API call, see
    -- <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
    --
    -- Currently, this parameter supports only failover region and account.
    additionalInfo :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)),
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
-- Create a value of 'DescribeAppVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalInfo', 'describeAppVersionResponse_additionalInfo' - Additional configuration parameters for an Resilience Hub application.
-- If you want to implement @additionalInfo@ through the Resilience Hub
-- console rather than using an API call, see
-- <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
--
-- Currently, this parameter supports only failover region and account.
--
-- 'httpStatus', 'describeAppVersionResponse_httpStatus' - The response's http status code.
--
-- 'appArn', 'describeAppVersionResponse_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'appVersion', 'describeAppVersionResponse_appVersion' - The Resilience Hub application version.
newDescribeAppVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  DescribeAppVersionResponse
newDescribeAppVersionResponse
  pHttpStatus_
  pAppArn_
  pAppVersion_ =
    DescribeAppVersionResponse'
      { additionalInfo =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        appArn = pAppArn_,
        appVersion = pAppVersion_
      }

-- | Additional configuration parameters for an Resilience Hub application.
-- If you want to implement @additionalInfo@ through the Resilience Hub
-- console rather than using an API call, see
-- <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
--
-- Currently, this parameter supports only failover region and account.
describeAppVersionResponse_additionalInfo :: Lens.Lens' DescribeAppVersionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)))
describeAppVersionResponse_additionalInfo = Lens.lens (\DescribeAppVersionResponse' {additionalInfo} -> additionalInfo) (\s@DescribeAppVersionResponse' {} a -> s {additionalInfo = a} :: DescribeAppVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAppVersionResponse_httpStatus :: Lens.Lens' DescribeAppVersionResponse Prelude.Int
describeAppVersionResponse_httpStatus = Lens.lens (\DescribeAppVersionResponse' {httpStatus} -> httpStatus) (\s@DescribeAppVersionResponse' {} a -> s {httpStatus = a} :: DescribeAppVersionResponse)

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
describeAppVersionResponse_appArn :: Lens.Lens' DescribeAppVersionResponse Prelude.Text
describeAppVersionResponse_appArn = Lens.lens (\DescribeAppVersionResponse' {appArn} -> appArn) (\s@DescribeAppVersionResponse' {} a -> s {appArn = a} :: DescribeAppVersionResponse)

-- | The Resilience Hub application version.
describeAppVersionResponse_appVersion :: Lens.Lens' DescribeAppVersionResponse Prelude.Text
describeAppVersionResponse_appVersion = Lens.lens (\DescribeAppVersionResponse' {appVersion} -> appVersion) (\s@DescribeAppVersionResponse' {} a -> s {appVersion = a} :: DescribeAppVersionResponse)

instance Prelude.NFData DescribeAppVersionResponse where
  rnf DescribeAppVersionResponse' {..} =
    Prelude.rnf additionalInfo
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion

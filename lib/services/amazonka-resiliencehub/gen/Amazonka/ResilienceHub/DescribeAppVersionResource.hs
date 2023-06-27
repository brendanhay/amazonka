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
-- Module      : Amazonka.ResilienceHub.DescribeAppVersionResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a resource of the Resilience Hub application.
--
-- This API accepts only one of the following parameters to descibe the
-- resource:
--
-- -   @resourceName@
--
-- -   @logicalResourceId@
--
-- -   @physicalResourceId@ (Along with @physicalResourceId@, you can also
--     provide @awsAccountId@, and @awsRegion@)
module Amazonka.ResilienceHub.DescribeAppVersionResource
  ( -- * Creating a Request
    DescribeAppVersionResource (..),
    newDescribeAppVersionResource,

    -- * Request Lenses
    describeAppVersionResource_awsAccountId,
    describeAppVersionResource_awsRegion,
    describeAppVersionResource_logicalResourceId,
    describeAppVersionResource_physicalResourceId,
    describeAppVersionResource_resourceName,
    describeAppVersionResource_appArn,
    describeAppVersionResource_appVersion,

    -- * Destructuring the Response
    DescribeAppVersionResourceResponse (..),
    newDescribeAppVersionResourceResponse,

    -- * Response Lenses
    describeAppVersionResourceResponse_physicalResource,
    describeAppVersionResourceResponse_httpStatus,
    describeAppVersionResourceResponse_appArn,
    describeAppVersionResourceResponse_appVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAppVersionResource' smart constructor.
data DescribeAppVersionResource = DescribeAppVersionResource'
  { -- | The Amazon Web Services account that owns the physical resource.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services region that owns the physical resource.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | The logical identifier of the resource.
    logicalResourceId :: Prelude.Maybe LogicalResourceId,
    -- | The physical identifier of the resource.
    physicalResourceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource.
    resourceName :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'DescribeAppVersionResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeAppVersionResource_awsAccountId' - The Amazon Web Services account that owns the physical resource.
--
-- 'awsRegion', 'describeAppVersionResource_awsRegion' - The Amazon Web Services region that owns the physical resource.
--
-- 'logicalResourceId', 'describeAppVersionResource_logicalResourceId' - The logical identifier of the resource.
--
-- 'physicalResourceId', 'describeAppVersionResource_physicalResourceId' - The physical identifier of the resource.
--
-- 'resourceName', 'describeAppVersionResource_resourceName' - The name of the resource.
--
-- 'appArn', 'describeAppVersionResource_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'appVersion', 'describeAppVersionResource_appVersion' - The Resilience Hub application version.
newDescribeAppVersionResource ::
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  DescribeAppVersionResource
newDescribeAppVersionResource pAppArn_ pAppVersion_ =
  DescribeAppVersionResource'
    { awsAccountId =
        Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      logicalResourceId = Prelude.Nothing,
      physicalResourceId = Prelude.Nothing,
      resourceName = Prelude.Nothing,
      appArn = pAppArn_,
      appVersion = pAppVersion_
    }

-- | The Amazon Web Services account that owns the physical resource.
describeAppVersionResource_awsAccountId :: Lens.Lens' DescribeAppVersionResource (Prelude.Maybe Prelude.Text)
describeAppVersionResource_awsAccountId = Lens.lens (\DescribeAppVersionResource' {awsAccountId} -> awsAccountId) (\s@DescribeAppVersionResource' {} a -> s {awsAccountId = a} :: DescribeAppVersionResource)

-- | The Amazon Web Services region that owns the physical resource.
describeAppVersionResource_awsRegion :: Lens.Lens' DescribeAppVersionResource (Prelude.Maybe Prelude.Text)
describeAppVersionResource_awsRegion = Lens.lens (\DescribeAppVersionResource' {awsRegion} -> awsRegion) (\s@DescribeAppVersionResource' {} a -> s {awsRegion = a} :: DescribeAppVersionResource)

-- | The logical identifier of the resource.
describeAppVersionResource_logicalResourceId :: Lens.Lens' DescribeAppVersionResource (Prelude.Maybe LogicalResourceId)
describeAppVersionResource_logicalResourceId = Lens.lens (\DescribeAppVersionResource' {logicalResourceId} -> logicalResourceId) (\s@DescribeAppVersionResource' {} a -> s {logicalResourceId = a} :: DescribeAppVersionResource)

-- | The physical identifier of the resource.
describeAppVersionResource_physicalResourceId :: Lens.Lens' DescribeAppVersionResource (Prelude.Maybe Prelude.Text)
describeAppVersionResource_physicalResourceId = Lens.lens (\DescribeAppVersionResource' {physicalResourceId} -> physicalResourceId) (\s@DescribeAppVersionResource' {} a -> s {physicalResourceId = a} :: DescribeAppVersionResource)

-- | The name of the resource.
describeAppVersionResource_resourceName :: Lens.Lens' DescribeAppVersionResource (Prelude.Maybe Prelude.Text)
describeAppVersionResource_resourceName = Lens.lens (\DescribeAppVersionResource' {resourceName} -> resourceName) (\s@DescribeAppVersionResource' {} a -> s {resourceName = a} :: DescribeAppVersionResource)

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
describeAppVersionResource_appArn :: Lens.Lens' DescribeAppVersionResource Prelude.Text
describeAppVersionResource_appArn = Lens.lens (\DescribeAppVersionResource' {appArn} -> appArn) (\s@DescribeAppVersionResource' {} a -> s {appArn = a} :: DescribeAppVersionResource)

-- | The Resilience Hub application version.
describeAppVersionResource_appVersion :: Lens.Lens' DescribeAppVersionResource Prelude.Text
describeAppVersionResource_appVersion = Lens.lens (\DescribeAppVersionResource' {appVersion} -> appVersion) (\s@DescribeAppVersionResource' {} a -> s {appVersion = a} :: DescribeAppVersionResource)

instance Core.AWSRequest DescribeAppVersionResource where
  type
    AWSResponse DescribeAppVersionResource =
      DescribeAppVersionResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAppVersionResourceResponse'
            Prelude.<$> (x Data..?> "physicalResource")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "appArn")
            Prelude.<*> (x Data..:> "appVersion")
      )

instance Prelude.Hashable DescribeAppVersionResource where
  hashWithSalt _salt DescribeAppVersionResource' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` logicalResourceId
      `Prelude.hashWithSalt` physicalResourceId
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` appArn
      `Prelude.hashWithSalt` appVersion

instance Prelude.NFData DescribeAppVersionResource where
  rnf DescribeAppVersionResource' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf logicalResourceId
      `Prelude.seq` Prelude.rnf physicalResourceId
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion

instance Data.ToHeaders DescribeAppVersionResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAppVersionResource where
  toJSON DescribeAppVersionResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("awsAccountId" Data..=) Prelude.<$> awsAccountId,
            ("awsRegion" Data..=) Prelude.<$> awsRegion,
            ("logicalResourceId" Data..=)
              Prelude.<$> logicalResourceId,
            ("physicalResourceId" Data..=)
              Prelude.<$> physicalResourceId,
            ("resourceName" Data..=) Prelude.<$> resourceName,
            Prelude.Just ("appArn" Data..= appArn),
            Prelude.Just ("appVersion" Data..= appVersion)
          ]
      )

instance Data.ToPath DescribeAppVersionResource where
  toPath =
    Prelude.const "/describe-app-version-resource"

instance Data.ToQuery DescribeAppVersionResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAppVersionResourceResponse' smart constructor.
data DescribeAppVersionResourceResponse = DescribeAppVersionResourceResponse'
  { -- | Defines a physical resource. A physical resource is a resource that
    -- exists in your account. It can be identified using an Amazon Resource
    -- Name (ARN) or a Resilience Hub-native identifier.
    physicalResource :: Prelude.Maybe PhysicalResource,
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
-- Create a value of 'DescribeAppVersionResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'physicalResource', 'describeAppVersionResourceResponse_physicalResource' - Defines a physical resource. A physical resource is a resource that
-- exists in your account. It can be identified using an Amazon Resource
-- Name (ARN) or a Resilience Hub-native identifier.
--
-- 'httpStatus', 'describeAppVersionResourceResponse_httpStatus' - The response's http status code.
--
-- 'appArn', 'describeAppVersionResourceResponse_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'appVersion', 'describeAppVersionResourceResponse_appVersion' - The Resilience Hub application version.
newDescribeAppVersionResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  DescribeAppVersionResourceResponse
newDescribeAppVersionResourceResponse
  pHttpStatus_
  pAppArn_
  pAppVersion_ =
    DescribeAppVersionResourceResponse'
      { physicalResource =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        appArn = pAppArn_,
        appVersion = pAppVersion_
      }

-- | Defines a physical resource. A physical resource is a resource that
-- exists in your account. It can be identified using an Amazon Resource
-- Name (ARN) or a Resilience Hub-native identifier.
describeAppVersionResourceResponse_physicalResource :: Lens.Lens' DescribeAppVersionResourceResponse (Prelude.Maybe PhysicalResource)
describeAppVersionResourceResponse_physicalResource = Lens.lens (\DescribeAppVersionResourceResponse' {physicalResource} -> physicalResource) (\s@DescribeAppVersionResourceResponse' {} a -> s {physicalResource = a} :: DescribeAppVersionResourceResponse)

-- | The response's http status code.
describeAppVersionResourceResponse_httpStatus :: Lens.Lens' DescribeAppVersionResourceResponse Prelude.Int
describeAppVersionResourceResponse_httpStatus = Lens.lens (\DescribeAppVersionResourceResponse' {httpStatus} -> httpStatus) (\s@DescribeAppVersionResourceResponse' {} a -> s {httpStatus = a} :: DescribeAppVersionResourceResponse)

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
describeAppVersionResourceResponse_appArn :: Lens.Lens' DescribeAppVersionResourceResponse Prelude.Text
describeAppVersionResourceResponse_appArn = Lens.lens (\DescribeAppVersionResourceResponse' {appArn} -> appArn) (\s@DescribeAppVersionResourceResponse' {} a -> s {appArn = a} :: DescribeAppVersionResourceResponse)

-- | The Resilience Hub application version.
describeAppVersionResourceResponse_appVersion :: Lens.Lens' DescribeAppVersionResourceResponse Prelude.Text
describeAppVersionResourceResponse_appVersion = Lens.lens (\DescribeAppVersionResourceResponse' {appVersion} -> appVersion) (\s@DescribeAppVersionResourceResponse' {} a -> s {appVersion = a} :: DescribeAppVersionResourceResponse)

instance
  Prelude.NFData
    DescribeAppVersionResourceResponse
  where
  rnf DescribeAppVersionResourceResponse' {..} =
    Prelude.rnf physicalResource
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion

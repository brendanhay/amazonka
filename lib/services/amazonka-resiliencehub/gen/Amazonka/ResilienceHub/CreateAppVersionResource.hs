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
-- Module      : Amazonka.ResilienceHub.CreateAppVersionResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a resource to the Resilience Hub application and assigns it to the
-- specified Application Components. If you specify a new Application
-- Component, Resilience Hub will automatically create the Application
-- Component.
--
-- -   This action has no effect outside Resilience Hub.
--
-- -   This API updates the Resilience Hub application draft version. To
--     use this resource for running resiliency assessments, you must
--     publish the Resilience Hub application using the @PublishAppVersion@
--     API.
--
-- -   To update application version with new @physicalResourceID@, you
--     must call @ResolveAppVersionResources@ API.
module Amazonka.ResilienceHub.CreateAppVersionResource
  ( -- * Creating a Request
    CreateAppVersionResource (..),
    newCreateAppVersionResource,

    -- * Request Lenses
    createAppVersionResource_additionalInfo,
    createAppVersionResource_awsAccountId,
    createAppVersionResource_awsRegion,
    createAppVersionResource_clientToken,
    createAppVersionResource_resourceName,
    createAppVersionResource_appArn,
    createAppVersionResource_appComponents,
    createAppVersionResource_logicalResourceId,
    createAppVersionResource_physicalResourceId,
    createAppVersionResource_resourceType,

    -- * Destructuring the Response
    CreateAppVersionResourceResponse (..),
    newCreateAppVersionResourceResponse,

    -- * Response Lenses
    createAppVersionResourceResponse_physicalResource,
    createAppVersionResourceResponse_httpStatus,
    createAppVersionResourceResponse_appArn,
    createAppVersionResourceResponse_appVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAppVersionResource' smart constructor.
data CreateAppVersionResource = CreateAppVersionResource'
  { -- | Currently, there is no supported additional information for resources.
    additionalInfo :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)),
    -- | The Amazon Web Services account that owns the physical resource.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services region that owns the physical resource.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | Used for an idempotency token. A client token is a unique,
    -- case-sensitive string of up to 64 ASCII characters. You should not reuse
    -- the same client token for other API requests.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
    -- format for this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
    -- information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    appArn :: Prelude.Text,
    -- | The list of Application Components that this resource belongs to. If an
    -- Application Component is not part of the Resilience Hub application, it
    -- will be added.
    appComponents :: [Prelude.Text],
    -- | The logical identifier of the resource.
    logicalResourceId :: LogicalResourceId,
    -- | The physical identifier of the resource.
    physicalResourceId :: Prelude.Text,
    -- | The type of resource.
    resourceType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAppVersionResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalInfo', 'createAppVersionResource_additionalInfo' - Currently, there is no supported additional information for resources.
--
-- 'awsAccountId', 'createAppVersionResource_awsAccountId' - The Amazon Web Services account that owns the physical resource.
--
-- 'awsRegion', 'createAppVersionResource_awsRegion' - The Amazon Web Services region that owns the physical resource.
--
-- 'clientToken', 'createAppVersionResource_clientToken' - Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
--
-- 'resourceName', 'createAppVersionResource_resourceName' - The name of the resource.
--
-- 'appArn', 'createAppVersionResource_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'appComponents', 'createAppVersionResource_appComponents' - The list of Application Components that this resource belongs to. If an
-- Application Component is not part of the Resilience Hub application, it
-- will be added.
--
-- 'logicalResourceId', 'createAppVersionResource_logicalResourceId' - The logical identifier of the resource.
--
-- 'physicalResourceId', 'createAppVersionResource_physicalResourceId' - The physical identifier of the resource.
--
-- 'resourceType', 'createAppVersionResource_resourceType' - The type of resource.
newCreateAppVersionResource ::
  -- | 'appArn'
  Prelude.Text ->
  -- | 'logicalResourceId'
  LogicalResourceId ->
  -- | 'physicalResourceId'
  Prelude.Text ->
  -- | 'resourceType'
  Prelude.Text ->
  CreateAppVersionResource
newCreateAppVersionResource
  pAppArn_
  pLogicalResourceId_
  pPhysicalResourceId_
  pResourceType_ =
    CreateAppVersionResource'
      { additionalInfo =
          Prelude.Nothing,
        awsAccountId = Prelude.Nothing,
        awsRegion = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        resourceName = Prelude.Nothing,
        appArn = pAppArn_,
        appComponents = Prelude.mempty,
        logicalResourceId = pLogicalResourceId_,
        physicalResourceId = pPhysicalResourceId_,
        resourceType = pResourceType_
      }

-- | Currently, there is no supported additional information for resources.
createAppVersionResource_additionalInfo :: Lens.Lens' CreateAppVersionResource (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)))
createAppVersionResource_additionalInfo = Lens.lens (\CreateAppVersionResource' {additionalInfo} -> additionalInfo) (\s@CreateAppVersionResource' {} a -> s {additionalInfo = a} :: CreateAppVersionResource) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services account that owns the physical resource.
createAppVersionResource_awsAccountId :: Lens.Lens' CreateAppVersionResource (Prelude.Maybe Prelude.Text)
createAppVersionResource_awsAccountId = Lens.lens (\CreateAppVersionResource' {awsAccountId} -> awsAccountId) (\s@CreateAppVersionResource' {} a -> s {awsAccountId = a} :: CreateAppVersionResource)

-- | The Amazon Web Services region that owns the physical resource.
createAppVersionResource_awsRegion :: Lens.Lens' CreateAppVersionResource (Prelude.Maybe Prelude.Text)
createAppVersionResource_awsRegion = Lens.lens (\CreateAppVersionResource' {awsRegion} -> awsRegion) (\s@CreateAppVersionResource' {} a -> s {awsRegion = a} :: CreateAppVersionResource)

-- | Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
createAppVersionResource_clientToken :: Lens.Lens' CreateAppVersionResource (Prelude.Maybe Prelude.Text)
createAppVersionResource_clientToken = Lens.lens (\CreateAppVersionResource' {clientToken} -> clientToken) (\s@CreateAppVersionResource' {} a -> s {clientToken = a} :: CreateAppVersionResource)

-- | The name of the resource.
createAppVersionResource_resourceName :: Lens.Lens' CreateAppVersionResource (Prelude.Maybe Prelude.Text)
createAppVersionResource_resourceName = Lens.lens (\CreateAppVersionResource' {resourceName} -> resourceName) (\s@CreateAppVersionResource' {} a -> s {resourceName = a} :: CreateAppVersionResource)

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
createAppVersionResource_appArn :: Lens.Lens' CreateAppVersionResource Prelude.Text
createAppVersionResource_appArn = Lens.lens (\CreateAppVersionResource' {appArn} -> appArn) (\s@CreateAppVersionResource' {} a -> s {appArn = a} :: CreateAppVersionResource)

-- | The list of Application Components that this resource belongs to. If an
-- Application Component is not part of the Resilience Hub application, it
-- will be added.
createAppVersionResource_appComponents :: Lens.Lens' CreateAppVersionResource [Prelude.Text]
createAppVersionResource_appComponents = Lens.lens (\CreateAppVersionResource' {appComponents} -> appComponents) (\s@CreateAppVersionResource' {} a -> s {appComponents = a} :: CreateAppVersionResource) Prelude.. Lens.coerced

-- | The logical identifier of the resource.
createAppVersionResource_logicalResourceId :: Lens.Lens' CreateAppVersionResource LogicalResourceId
createAppVersionResource_logicalResourceId = Lens.lens (\CreateAppVersionResource' {logicalResourceId} -> logicalResourceId) (\s@CreateAppVersionResource' {} a -> s {logicalResourceId = a} :: CreateAppVersionResource)

-- | The physical identifier of the resource.
createAppVersionResource_physicalResourceId :: Lens.Lens' CreateAppVersionResource Prelude.Text
createAppVersionResource_physicalResourceId = Lens.lens (\CreateAppVersionResource' {physicalResourceId} -> physicalResourceId) (\s@CreateAppVersionResource' {} a -> s {physicalResourceId = a} :: CreateAppVersionResource)

-- | The type of resource.
createAppVersionResource_resourceType :: Lens.Lens' CreateAppVersionResource Prelude.Text
createAppVersionResource_resourceType = Lens.lens (\CreateAppVersionResource' {resourceType} -> resourceType) (\s@CreateAppVersionResource' {} a -> s {resourceType = a} :: CreateAppVersionResource)

instance Core.AWSRequest CreateAppVersionResource where
  type
    AWSResponse CreateAppVersionResource =
      CreateAppVersionResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAppVersionResourceResponse'
            Prelude.<$> (x Data..?> "physicalResource")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "appArn")
            Prelude.<*> (x Data..:> "appVersion")
      )

instance Prelude.Hashable CreateAppVersionResource where
  hashWithSalt _salt CreateAppVersionResource' {..} =
    _salt
      `Prelude.hashWithSalt` additionalInfo
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` appArn
      `Prelude.hashWithSalt` appComponents
      `Prelude.hashWithSalt` logicalResourceId
      `Prelude.hashWithSalt` physicalResourceId
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData CreateAppVersionResource where
  rnf CreateAppVersionResource' {..} =
    Prelude.rnf additionalInfo
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appComponents
      `Prelude.seq` Prelude.rnf logicalResourceId
      `Prelude.seq` Prelude.rnf physicalResourceId
      `Prelude.seq` Prelude.rnf resourceType

instance Data.ToHeaders CreateAppVersionResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAppVersionResource where
  toJSON CreateAppVersionResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("additionalInfo" Data..=)
              Prelude.<$> additionalInfo,
            ("awsAccountId" Data..=) Prelude.<$> awsAccountId,
            ("awsRegion" Data..=) Prelude.<$> awsRegion,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("resourceName" Data..=) Prelude.<$> resourceName,
            Prelude.Just ("appArn" Data..= appArn),
            Prelude.Just ("appComponents" Data..= appComponents),
            Prelude.Just
              ("logicalResourceId" Data..= logicalResourceId),
            Prelude.Just
              ("physicalResourceId" Data..= physicalResourceId),
            Prelude.Just ("resourceType" Data..= resourceType)
          ]
      )

instance Data.ToPath CreateAppVersionResource where
  toPath = Prelude.const "/create-app-version-resource"

instance Data.ToQuery CreateAppVersionResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAppVersionResourceResponse' smart constructor.
data CreateAppVersionResourceResponse = CreateAppVersionResourceResponse'
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
-- Create a value of 'CreateAppVersionResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'physicalResource', 'createAppVersionResourceResponse_physicalResource' - Defines a physical resource. A physical resource is a resource that
-- exists in your account. It can be identified using an Amazon Resource
-- Name (ARN) or a Resilience Hub-native identifier.
--
-- 'httpStatus', 'createAppVersionResourceResponse_httpStatus' - The response's http status code.
--
-- 'appArn', 'createAppVersionResourceResponse_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'appVersion', 'createAppVersionResourceResponse_appVersion' - The Resilience Hub application version.
newCreateAppVersionResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  CreateAppVersionResourceResponse
newCreateAppVersionResourceResponse
  pHttpStatus_
  pAppArn_
  pAppVersion_ =
    CreateAppVersionResourceResponse'
      { physicalResource =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        appArn = pAppArn_,
        appVersion = pAppVersion_
      }

-- | Defines a physical resource. A physical resource is a resource that
-- exists in your account. It can be identified using an Amazon Resource
-- Name (ARN) or a Resilience Hub-native identifier.
createAppVersionResourceResponse_physicalResource :: Lens.Lens' CreateAppVersionResourceResponse (Prelude.Maybe PhysicalResource)
createAppVersionResourceResponse_physicalResource = Lens.lens (\CreateAppVersionResourceResponse' {physicalResource} -> physicalResource) (\s@CreateAppVersionResourceResponse' {} a -> s {physicalResource = a} :: CreateAppVersionResourceResponse)

-- | The response's http status code.
createAppVersionResourceResponse_httpStatus :: Lens.Lens' CreateAppVersionResourceResponse Prelude.Int
createAppVersionResourceResponse_httpStatus = Lens.lens (\CreateAppVersionResourceResponse' {httpStatus} -> httpStatus) (\s@CreateAppVersionResourceResponse' {} a -> s {httpStatus = a} :: CreateAppVersionResourceResponse)

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
createAppVersionResourceResponse_appArn :: Lens.Lens' CreateAppVersionResourceResponse Prelude.Text
createAppVersionResourceResponse_appArn = Lens.lens (\CreateAppVersionResourceResponse' {appArn} -> appArn) (\s@CreateAppVersionResourceResponse' {} a -> s {appArn = a} :: CreateAppVersionResourceResponse)

-- | The Resilience Hub application version.
createAppVersionResourceResponse_appVersion :: Lens.Lens' CreateAppVersionResourceResponse Prelude.Text
createAppVersionResourceResponse_appVersion = Lens.lens (\CreateAppVersionResourceResponse' {appVersion} -> appVersion) (\s@CreateAppVersionResourceResponse' {} a -> s {appVersion = a} :: CreateAppVersionResourceResponse)

instance
  Prelude.NFData
    CreateAppVersionResourceResponse
  where
  rnf CreateAppVersionResourceResponse' {..} =
    Prelude.rnf physicalResource
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion

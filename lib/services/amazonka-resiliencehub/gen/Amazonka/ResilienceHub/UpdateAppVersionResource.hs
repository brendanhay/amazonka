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
-- Module      : Amazonka.ResilienceHub.UpdateAppVersionResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the resource details in the Resilience Hub application.
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
module Amazonka.ResilienceHub.UpdateAppVersionResource
  ( -- * Creating a Request
    UpdateAppVersionResource (..),
    newUpdateAppVersionResource,

    -- * Request Lenses
    updateAppVersionResource_additionalInfo,
    updateAppVersionResource_appComponents,
    updateAppVersionResource_awsAccountId,
    updateAppVersionResource_awsRegion,
    updateAppVersionResource_excluded,
    updateAppVersionResource_logicalResourceId,
    updateAppVersionResource_physicalResourceId,
    updateAppVersionResource_resourceName,
    updateAppVersionResource_resourceType,
    updateAppVersionResource_appArn,

    -- * Destructuring the Response
    UpdateAppVersionResourceResponse (..),
    newUpdateAppVersionResourceResponse,

    -- * Response Lenses
    updateAppVersionResourceResponse_physicalResource,
    updateAppVersionResourceResponse_httpStatus,
    updateAppVersionResourceResponse_appArn,
    updateAppVersionResourceResponse_appVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAppVersionResource' smart constructor.
data UpdateAppVersionResource = UpdateAppVersionResource'
  { -- | Currently, there is no supported additional information for resources.
    additionalInfo :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)),
    -- | The list of Application Components that this resource belongs to. If an
    -- Application Component is not part of the Resilience Hub application, it
    -- will be added.
    appComponents :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Web Services account that owns the physical resource.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services region that owns the physical resource.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | Indicates if a resource is excluded from an Resilience Hub application.
    --
    -- You can exclude only imported resources from an Resilience Hub
    -- application.
    excluded :: Prelude.Maybe Prelude.Bool,
    -- | The logical identifier of the resource.
    logicalResourceId :: Prelude.Maybe LogicalResourceId,
    -- | The physical identifier of the resource.
    physicalResourceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The type of resource.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
    -- format for this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
    -- information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    appArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAppVersionResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalInfo', 'updateAppVersionResource_additionalInfo' - Currently, there is no supported additional information for resources.
--
-- 'appComponents', 'updateAppVersionResource_appComponents' - The list of Application Components that this resource belongs to. If an
-- Application Component is not part of the Resilience Hub application, it
-- will be added.
--
-- 'awsAccountId', 'updateAppVersionResource_awsAccountId' - The Amazon Web Services account that owns the physical resource.
--
-- 'awsRegion', 'updateAppVersionResource_awsRegion' - The Amazon Web Services region that owns the physical resource.
--
-- 'excluded', 'updateAppVersionResource_excluded' - Indicates if a resource is excluded from an Resilience Hub application.
--
-- You can exclude only imported resources from an Resilience Hub
-- application.
--
-- 'logicalResourceId', 'updateAppVersionResource_logicalResourceId' - The logical identifier of the resource.
--
-- 'physicalResourceId', 'updateAppVersionResource_physicalResourceId' - The physical identifier of the resource.
--
-- 'resourceName', 'updateAppVersionResource_resourceName' - The name of the resource.
--
-- 'resourceType', 'updateAppVersionResource_resourceType' - The type of resource.
--
-- 'appArn', 'updateAppVersionResource_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
newUpdateAppVersionResource ::
  -- | 'appArn'
  Prelude.Text ->
  UpdateAppVersionResource
newUpdateAppVersionResource pAppArn_ =
  UpdateAppVersionResource'
    { additionalInfo =
        Prelude.Nothing,
      appComponents = Prelude.Nothing,
      awsAccountId = Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      excluded = Prelude.Nothing,
      logicalResourceId = Prelude.Nothing,
      physicalResourceId = Prelude.Nothing,
      resourceName = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      appArn = pAppArn_
    }

-- | Currently, there is no supported additional information for resources.
updateAppVersionResource_additionalInfo :: Lens.Lens' UpdateAppVersionResource (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)))
updateAppVersionResource_additionalInfo = Lens.lens (\UpdateAppVersionResource' {additionalInfo} -> additionalInfo) (\s@UpdateAppVersionResource' {} a -> s {additionalInfo = a} :: UpdateAppVersionResource) Prelude.. Lens.mapping Lens.coerced

-- | The list of Application Components that this resource belongs to. If an
-- Application Component is not part of the Resilience Hub application, it
-- will be added.
updateAppVersionResource_appComponents :: Lens.Lens' UpdateAppVersionResource (Prelude.Maybe [Prelude.Text])
updateAppVersionResource_appComponents = Lens.lens (\UpdateAppVersionResource' {appComponents} -> appComponents) (\s@UpdateAppVersionResource' {} a -> s {appComponents = a} :: UpdateAppVersionResource) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services account that owns the physical resource.
updateAppVersionResource_awsAccountId :: Lens.Lens' UpdateAppVersionResource (Prelude.Maybe Prelude.Text)
updateAppVersionResource_awsAccountId = Lens.lens (\UpdateAppVersionResource' {awsAccountId} -> awsAccountId) (\s@UpdateAppVersionResource' {} a -> s {awsAccountId = a} :: UpdateAppVersionResource)

-- | The Amazon Web Services region that owns the physical resource.
updateAppVersionResource_awsRegion :: Lens.Lens' UpdateAppVersionResource (Prelude.Maybe Prelude.Text)
updateAppVersionResource_awsRegion = Lens.lens (\UpdateAppVersionResource' {awsRegion} -> awsRegion) (\s@UpdateAppVersionResource' {} a -> s {awsRegion = a} :: UpdateAppVersionResource)

-- | Indicates if a resource is excluded from an Resilience Hub application.
--
-- You can exclude only imported resources from an Resilience Hub
-- application.
updateAppVersionResource_excluded :: Lens.Lens' UpdateAppVersionResource (Prelude.Maybe Prelude.Bool)
updateAppVersionResource_excluded = Lens.lens (\UpdateAppVersionResource' {excluded} -> excluded) (\s@UpdateAppVersionResource' {} a -> s {excluded = a} :: UpdateAppVersionResource)

-- | The logical identifier of the resource.
updateAppVersionResource_logicalResourceId :: Lens.Lens' UpdateAppVersionResource (Prelude.Maybe LogicalResourceId)
updateAppVersionResource_logicalResourceId = Lens.lens (\UpdateAppVersionResource' {logicalResourceId} -> logicalResourceId) (\s@UpdateAppVersionResource' {} a -> s {logicalResourceId = a} :: UpdateAppVersionResource)

-- | The physical identifier of the resource.
updateAppVersionResource_physicalResourceId :: Lens.Lens' UpdateAppVersionResource (Prelude.Maybe Prelude.Text)
updateAppVersionResource_physicalResourceId = Lens.lens (\UpdateAppVersionResource' {physicalResourceId} -> physicalResourceId) (\s@UpdateAppVersionResource' {} a -> s {physicalResourceId = a} :: UpdateAppVersionResource)

-- | The name of the resource.
updateAppVersionResource_resourceName :: Lens.Lens' UpdateAppVersionResource (Prelude.Maybe Prelude.Text)
updateAppVersionResource_resourceName = Lens.lens (\UpdateAppVersionResource' {resourceName} -> resourceName) (\s@UpdateAppVersionResource' {} a -> s {resourceName = a} :: UpdateAppVersionResource)

-- | The type of resource.
updateAppVersionResource_resourceType :: Lens.Lens' UpdateAppVersionResource (Prelude.Maybe Prelude.Text)
updateAppVersionResource_resourceType = Lens.lens (\UpdateAppVersionResource' {resourceType} -> resourceType) (\s@UpdateAppVersionResource' {} a -> s {resourceType = a} :: UpdateAppVersionResource)

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
updateAppVersionResource_appArn :: Lens.Lens' UpdateAppVersionResource Prelude.Text
updateAppVersionResource_appArn = Lens.lens (\UpdateAppVersionResource' {appArn} -> appArn) (\s@UpdateAppVersionResource' {} a -> s {appArn = a} :: UpdateAppVersionResource)

instance Core.AWSRequest UpdateAppVersionResource where
  type
    AWSResponse UpdateAppVersionResource =
      UpdateAppVersionResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAppVersionResourceResponse'
            Prelude.<$> (x Data..?> "physicalResource")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "appArn")
            Prelude.<*> (x Data..:> "appVersion")
      )

instance Prelude.Hashable UpdateAppVersionResource where
  hashWithSalt _salt UpdateAppVersionResource' {..} =
    _salt
      `Prelude.hashWithSalt` additionalInfo
      `Prelude.hashWithSalt` appComponents
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` excluded
      `Prelude.hashWithSalt` logicalResourceId
      `Prelude.hashWithSalt` physicalResourceId
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` appArn

instance Prelude.NFData UpdateAppVersionResource where
  rnf UpdateAppVersionResource' {..} =
    Prelude.rnf additionalInfo
      `Prelude.seq` Prelude.rnf appComponents
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf excluded
      `Prelude.seq` Prelude.rnf logicalResourceId
      `Prelude.seq` Prelude.rnf physicalResourceId
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf appArn

instance Data.ToHeaders UpdateAppVersionResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAppVersionResource where
  toJSON UpdateAppVersionResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("additionalInfo" Data..=)
              Prelude.<$> additionalInfo,
            ("appComponents" Data..=) Prelude.<$> appComponents,
            ("awsAccountId" Data..=) Prelude.<$> awsAccountId,
            ("awsRegion" Data..=) Prelude.<$> awsRegion,
            ("excluded" Data..=) Prelude.<$> excluded,
            ("logicalResourceId" Data..=)
              Prelude.<$> logicalResourceId,
            ("physicalResourceId" Data..=)
              Prelude.<$> physicalResourceId,
            ("resourceName" Data..=) Prelude.<$> resourceName,
            ("resourceType" Data..=) Prelude.<$> resourceType,
            Prelude.Just ("appArn" Data..= appArn)
          ]
      )

instance Data.ToPath UpdateAppVersionResource where
  toPath = Prelude.const "/update-app-version-resource"

instance Data.ToQuery UpdateAppVersionResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAppVersionResourceResponse' smart constructor.
data UpdateAppVersionResourceResponse = UpdateAppVersionResourceResponse'
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
-- Create a value of 'UpdateAppVersionResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'physicalResource', 'updateAppVersionResourceResponse_physicalResource' - Defines a physical resource. A physical resource is a resource that
-- exists in your account. It can be identified using an Amazon Resource
-- Name (ARN) or a Resilience Hub-native identifier.
--
-- 'httpStatus', 'updateAppVersionResourceResponse_httpStatus' - The response's http status code.
--
-- 'appArn', 'updateAppVersionResourceResponse_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'appVersion', 'updateAppVersionResourceResponse_appVersion' - The Resilience Hub application version.
newUpdateAppVersionResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  UpdateAppVersionResourceResponse
newUpdateAppVersionResourceResponse
  pHttpStatus_
  pAppArn_
  pAppVersion_ =
    UpdateAppVersionResourceResponse'
      { physicalResource =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        appArn = pAppArn_,
        appVersion = pAppVersion_
      }

-- | Defines a physical resource. A physical resource is a resource that
-- exists in your account. It can be identified using an Amazon Resource
-- Name (ARN) or a Resilience Hub-native identifier.
updateAppVersionResourceResponse_physicalResource :: Lens.Lens' UpdateAppVersionResourceResponse (Prelude.Maybe PhysicalResource)
updateAppVersionResourceResponse_physicalResource = Lens.lens (\UpdateAppVersionResourceResponse' {physicalResource} -> physicalResource) (\s@UpdateAppVersionResourceResponse' {} a -> s {physicalResource = a} :: UpdateAppVersionResourceResponse)

-- | The response's http status code.
updateAppVersionResourceResponse_httpStatus :: Lens.Lens' UpdateAppVersionResourceResponse Prelude.Int
updateAppVersionResourceResponse_httpStatus = Lens.lens (\UpdateAppVersionResourceResponse' {httpStatus} -> httpStatus) (\s@UpdateAppVersionResourceResponse' {} a -> s {httpStatus = a} :: UpdateAppVersionResourceResponse)

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
updateAppVersionResourceResponse_appArn :: Lens.Lens' UpdateAppVersionResourceResponse Prelude.Text
updateAppVersionResourceResponse_appArn = Lens.lens (\UpdateAppVersionResourceResponse' {appArn} -> appArn) (\s@UpdateAppVersionResourceResponse' {} a -> s {appArn = a} :: UpdateAppVersionResourceResponse)

-- | The Resilience Hub application version.
updateAppVersionResourceResponse_appVersion :: Lens.Lens' UpdateAppVersionResourceResponse Prelude.Text
updateAppVersionResourceResponse_appVersion = Lens.lens (\UpdateAppVersionResourceResponse' {appVersion} -> appVersion) (\s@UpdateAppVersionResourceResponse' {} a -> s {appVersion = a} :: UpdateAppVersionResourceResponse)

instance
  Prelude.NFData
    UpdateAppVersionResourceResponse
  where
  rnf UpdateAppVersionResourceResponse' {..} =
    Prelude.rnf physicalResource
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion

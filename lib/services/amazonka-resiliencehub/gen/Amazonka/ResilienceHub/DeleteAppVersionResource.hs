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
-- Module      : Amazonka.ResilienceHub.DeleteAppVersionResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a resource from the Resilience Hub application.
--
-- -   You can only delete a manually added resource. To exclude
--     non-manually added resources, use the @UpdateAppVersionResource@
--     API.
--
-- -   This action has no effect outside Resilience Hub.
--
-- -   This API updates the Resilience Hub application draft version. To
--     use this resource for running resiliency assessments, you must
--     publish the Resilience Hub application using the @PublishAppVersion@
--     API.
module Amazonka.ResilienceHub.DeleteAppVersionResource
  ( -- * Creating a Request
    DeleteAppVersionResource (..),
    newDeleteAppVersionResource,

    -- * Request Lenses
    deleteAppVersionResource_awsAccountId,
    deleteAppVersionResource_awsRegion,
    deleteAppVersionResource_clientToken,
    deleteAppVersionResource_logicalResourceId,
    deleteAppVersionResource_physicalResourceId,
    deleteAppVersionResource_resourceName,
    deleteAppVersionResource_appArn,

    -- * Destructuring the Response
    DeleteAppVersionResourceResponse (..),
    newDeleteAppVersionResourceResponse,

    -- * Response Lenses
    deleteAppVersionResourceResponse_physicalResource,
    deleteAppVersionResourceResponse_httpStatus,
    deleteAppVersionResourceResponse_appArn,
    deleteAppVersionResourceResponse_appVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAppVersionResource' smart constructor.
data DeleteAppVersionResource = DeleteAppVersionResource'
  { -- | The Amazon Web Services account that owns the physical resource.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services region that owns the physical resource.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | Used for an idempotency token. A client token is a unique,
    -- case-sensitive string of up to 64 ASCII characters. You should not reuse
    -- the same client token for other API requests.
    clientToken :: Prelude.Maybe Prelude.Text,
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
    appArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppVersionResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'deleteAppVersionResource_awsAccountId' - The Amazon Web Services account that owns the physical resource.
--
-- 'awsRegion', 'deleteAppVersionResource_awsRegion' - The Amazon Web Services region that owns the physical resource.
--
-- 'clientToken', 'deleteAppVersionResource_clientToken' - Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
--
-- 'logicalResourceId', 'deleteAppVersionResource_logicalResourceId' - The logical identifier of the resource.
--
-- 'physicalResourceId', 'deleteAppVersionResource_physicalResourceId' - The physical identifier of the resource.
--
-- 'resourceName', 'deleteAppVersionResource_resourceName' - The name of the resource.
--
-- 'appArn', 'deleteAppVersionResource_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
newDeleteAppVersionResource ::
  -- | 'appArn'
  Prelude.Text ->
  DeleteAppVersionResource
newDeleteAppVersionResource pAppArn_ =
  DeleteAppVersionResource'
    { awsAccountId =
        Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      logicalResourceId = Prelude.Nothing,
      physicalResourceId = Prelude.Nothing,
      resourceName = Prelude.Nothing,
      appArn = pAppArn_
    }

-- | The Amazon Web Services account that owns the physical resource.
deleteAppVersionResource_awsAccountId :: Lens.Lens' DeleteAppVersionResource (Prelude.Maybe Prelude.Text)
deleteAppVersionResource_awsAccountId = Lens.lens (\DeleteAppVersionResource' {awsAccountId} -> awsAccountId) (\s@DeleteAppVersionResource' {} a -> s {awsAccountId = a} :: DeleteAppVersionResource)

-- | The Amazon Web Services region that owns the physical resource.
deleteAppVersionResource_awsRegion :: Lens.Lens' DeleteAppVersionResource (Prelude.Maybe Prelude.Text)
deleteAppVersionResource_awsRegion = Lens.lens (\DeleteAppVersionResource' {awsRegion} -> awsRegion) (\s@DeleteAppVersionResource' {} a -> s {awsRegion = a} :: DeleteAppVersionResource)

-- | Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
deleteAppVersionResource_clientToken :: Lens.Lens' DeleteAppVersionResource (Prelude.Maybe Prelude.Text)
deleteAppVersionResource_clientToken = Lens.lens (\DeleteAppVersionResource' {clientToken} -> clientToken) (\s@DeleteAppVersionResource' {} a -> s {clientToken = a} :: DeleteAppVersionResource)

-- | The logical identifier of the resource.
deleteAppVersionResource_logicalResourceId :: Lens.Lens' DeleteAppVersionResource (Prelude.Maybe LogicalResourceId)
deleteAppVersionResource_logicalResourceId = Lens.lens (\DeleteAppVersionResource' {logicalResourceId} -> logicalResourceId) (\s@DeleteAppVersionResource' {} a -> s {logicalResourceId = a} :: DeleteAppVersionResource)

-- | The physical identifier of the resource.
deleteAppVersionResource_physicalResourceId :: Lens.Lens' DeleteAppVersionResource (Prelude.Maybe Prelude.Text)
deleteAppVersionResource_physicalResourceId = Lens.lens (\DeleteAppVersionResource' {physicalResourceId} -> physicalResourceId) (\s@DeleteAppVersionResource' {} a -> s {physicalResourceId = a} :: DeleteAppVersionResource)

-- | The name of the resource.
deleteAppVersionResource_resourceName :: Lens.Lens' DeleteAppVersionResource (Prelude.Maybe Prelude.Text)
deleteAppVersionResource_resourceName = Lens.lens (\DeleteAppVersionResource' {resourceName} -> resourceName) (\s@DeleteAppVersionResource' {} a -> s {resourceName = a} :: DeleteAppVersionResource)

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
deleteAppVersionResource_appArn :: Lens.Lens' DeleteAppVersionResource Prelude.Text
deleteAppVersionResource_appArn = Lens.lens (\DeleteAppVersionResource' {appArn} -> appArn) (\s@DeleteAppVersionResource' {} a -> s {appArn = a} :: DeleteAppVersionResource)

instance Core.AWSRequest DeleteAppVersionResource where
  type
    AWSResponse DeleteAppVersionResource =
      DeleteAppVersionResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAppVersionResourceResponse'
            Prelude.<$> (x Data..?> "physicalResource")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "appArn")
            Prelude.<*> (x Data..:> "appVersion")
      )

instance Prelude.Hashable DeleteAppVersionResource where
  hashWithSalt _salt DeleteAppVersionResource' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` logicalResourceId
      `Prelude.hashWithSalt` physicalResourceId
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` appArn

instance Prelude.NFData DeleteAppVersionResource where
  rnf DeleteAppVersionResource' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf logicalResourceId
      `Prelude.seq` Prelude.rnf physicalResourceId
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf appArn

instance Data.ToHeaders DeleteAppVersionResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAppVersionResource where
  toJSON DeleteAppVersionResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("awsAccountId" Data..=) Prelude.<$> awsAccountId,
            ("awsRegion" Data..=) Prelude.<$> awsRegion,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("logicalResourceId" Data..=)
              Prelude.<$> logicalResourceId,
            ("physicalResourceId" Data..=)
              Prelude.<$> physicalResourceId,
            ("resourceName" Data..=) Prelude.<$> resourceName,
            Prelude.Just ("appArn" Data..= appArn)
          ]
      )

instance Data.ToPath DeleteAppVersionResource where
  toPath = Prelude.const "/delete-app-version-resource"

instance Data.ToQuery DeleteAppVersionResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAppVersionResourceResponse' smart constructor.
data DeleteAppVersionResourceResponse = DeleteAppVersionResourceResponse'
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
-- Create a value of 'DeleteAppVersionResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'physicalResource', 'deleteAppVersionResourceResponse_physicalResource' - Defines a physical resource. A physical resource is a resource that
-- exists in your account. It can be identified using an Amazon Resource
-- Name (ARN) or a Resilience Hub-native identifier.
--
-- 'httpStatus', 'deleteAppVersionResourceResponse_httpStatus' - The response's http status code.
--
-- 'appArn', 'deleteAppVersionResourceResponse_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'appVersion', 'deleteAppVersionResourceResponse_appVersion' - The Resilience Hub application version.
newDeleteAppVersionResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  DeleteAppVersionResourceResponse
newDeleteAppVersionResourceResponse
  pHttpStatus_
  pAppArn_
  pAppVersion_ =
    DeleteAppVersionResourceResponse'
      { physicalResource =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        appArn = pAppArn_,
        appVersion = pAppVersion_
      }

-- | Defines a physical resource. A physical resource is a resource that
-- exists in your account. It can be identified using an Amazon Resource
-- Name (ARN) or a Resilience Hub-native identifier.
deleteAppVersionResourceResponse_physicalResource :: Lens.Lens' DeleteAppVersionResourceResponse (Prelude.Maybe PhysicalResource)
deleteAppVersionResourceResponse_physicalResource = Lens.lens (\DeleteAppVersionResourceResponse' {physicalResource} -> physicalResource) (\s@DeleteAppVersionResourceResponse' {} a -> s {physicalResource = a} :: DeleteAppVersionResourceResponse)

-- | The response's http status code.
deleteAppVersionResourceResponse_httpStatus :: Lens.Lens' DeleteAppVersionResourceResponse Prelude.Int
deleteAppVersionResourceResponse_httpStatus = Lens.lens (\DeleteAppVersionResourceResponse' {httpStatus} -> httpStatus) (\s@DeleteAppVersionResourceResponse' {} a -> s {httpStatus = a} :: DeleteAppVersionResourceResponse)

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
deleteAppVersionResourceResponse_appArn :: Lens.Lens' DeleteAppVersionResourceResponse Prelude.Text
deleteAppVersionResourceResponse_appArn = Lens.lens (\DeleteAppVersionResourceResponse' {appArn} -> appArn) (\s@DeleteAppVersionResourceResponse' {} a -> s {appArn = a} :: DeleteAppVersionResourceResponse)

-- | The Resilience Hub application version.
deleteAppVersionResourceResponse_appVersion :: Lens.Lens' DeleteAppVersionResourceResponse Prelude.Text
deleteAppVersionResourceResponse_appVersion = Lens.lens (\DeleteAppVersionResourceResponse' {appVersion} -> appVersion) (\s@DeleteAppVersionResourceResponse' {} a -> s {appVersion = a} :: DeleteAppVersionResourceResponse)

instance
  Prelude.NFData
    DeleteAppVersionResourceResponse
  where
  rnf DeleteAppVersionResourceResponse' {..} =
    Prelude.rnf physicalResource
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion

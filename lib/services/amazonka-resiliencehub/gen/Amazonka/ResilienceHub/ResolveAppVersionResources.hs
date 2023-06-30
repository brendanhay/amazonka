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
-- Module      : Amazonka.ResilienceHub.ResolveAppVersionResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resolves the resources for an application version.
module Amazonka.ResilienceHub.ResolveAppVersionResources
  ( -- * Creating a Request
    ResolveAppVersionResources (..),
    newResolveAppVersionResources,

    -- * Request Lenses
    resolveAppVersionResources_appArn,
    resolveAppVersionResources_appVersion,

    -- * Destructuring the Response
    ResolveAppVersionResourcesResponse (..),
    newResolveAppVersionResourcesResponse,

    -- * Response Lenses
    resolveAppVersionResourcesResponse_httpStatus,
    resolveAppVersionResourcesResponse_appArn,
    resolveAppVersionResourcesResponse_appVersion,
    resolveAppVersionResourcesResponse_resolutionId,
    resolveAppVersionResourcesResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newResolveAppVersionResources' smart constructor.
data ResolveAppVersionResources = ResolveAppVersionResources'
  { -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Text,
    -- | The version of the application.
    appVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResolveAppVersionResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appArn', 'resolveAppVersionResources_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'appVersion', 'resolveAppVersionResources_appVersion' - The version of the application.
newResolveAppVersionResources ::
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  ResolveAppVersionResources
newResolveAppVersionResources pAppArn_ pAppVersion_ =
  ResolveAppVersionResources'
    { appArn = pAppArn_,
      appVersion = pAppVersion_
    }

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
resolveAppVersionResources_appArn :: Lens.Lens' ResolveAppVersionResources Prelude.Text
resolveAppVersionResources_appArn = Lens.lens (\ResolveAppVersionResources' {appArn} -> appArn) (\s@ResolveAppVersionResources' {} a -> s {appArn = a} :: ResolveAppVersionResources)

-- | The version of the application.
resolveAppVersionResources_appVersion :: Lens.Lens' ResolveAppVersionResources Prelude.Text
resolveAppVersionResources_appVersion = Lens.lens (\ResolveAppVersionResources' {appVersion} -> appVersion) (\s@ResolveAppVersionResources' {} a -> s {appVersion = a} :: ResolveAppVersionResources)

instance Core.AWSRequest ResolveAppVersionResources where
  type
    AWSResponse ResolveAppVersionResources =
      ResolveAppVersionResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ResolveAppVersionResourcesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "appArn")
            Prelude.<*> (x Data..:> "appVersion")
            Prelude.<*> (x Data..:> "resolutionId")
            Prelude.<*> (x Data..:> "status")
      )

instance Prelude.Hashable ResolveAppVersionResources where
  hashWithSalt _salt ResolveAppVersionResources' {..} =
    _salt
      `Prelude.hashWithSalt` appArn
      `Prelude.hashWithSalt` appVersion

instance Prelude.NFData ResolveAppVersionResources where
  rnf ResolveAppVersionResources' {..} =
    Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion

instance Data.ToHeaders ResolveAppVersionResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ResolveAppVersionResources where
  toJSON ResolveAppVersionResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("appArn" Data..= appArn),
            Prelude.Just ("appVersion" Data..= appVersion)
          ]
      )

instance Data.ToPath ResolveAppVersionResources where
  toPath =
    Prelude.const "/resolve-app-version-resources"

instance Data.ToQuery ResolveAppVersionResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResolveAppVersionResourcesResponse' smart constructor.
data ResolveAppVersionResourcesResponse = ResolveAppVersionResourcesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Text,
    -- | The version of the application.
    appVersion :: Prelude.Text,
    -- | The identifier for a specific resolution.
    resolutionId :: Prelude.Text,
    -- | The status of the action.
    status :: ResourceResolutionStatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResolveAppVersionResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'resolveAppVersionResourcesResponse_httpStatus' - The response's http status code.
--
-- 'appArn', 'resolveAppVersionResourcesResponse_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'appVersion', 'resolveAppVersionResourcesResponse_appVersion' - The version of the application.
--
-- 'resolutionId', 'resolveAppVersionResourcesResponse_resolutionId' - The identifier for a specific resolution.
--
-- 'status', 'resolveAppVersionResourcesResponse_status' - The status of the action.
newResolveAppVersionResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  -- | 'resolutionId'
  Prelude.Text ->
  -- | 'status'
  ResourceResolutionStatusType ->
  ResolveAppVersionResourcesResponse
newResolveAppVersionResourcesResponse
  pHttpStatus_
  pAppArn_
  pAppVersion_
  pResolutionId_
  pStatus_ =
    ResolveAppVersionResourcesResponse'
      { httpStatus =
          pHttpStatus_,
        appArn = pAppArn_,
        appVersion = pAppVersion_,
        resolutionId = pResolutionId_,
        status = pStatus_
      }

-- | The response's http status code.
resolveAppVersionResourcesResponse_httpStatus :: Lens.Lens' ResolveAppVersionResourcesResponse Prelude.Int
resolveAppVersionResourcesResponse_httpStatus = Lens.lens (\ResolveAppVersionResourcesResponse' {httpStatus} -> httpStatus) (\s@ResolveAppVersionResourcesResponse' {} a -> s {httpStatus = a} :: ResolveAppVersionResourcesResponse)

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
resolveAppVersionResourcesResponse_appArn :: Lens.Lens' ResolveAppVersionResourcesResponse Prelude.Text
resolveAppVersionResourcesResponse_appArn = Lens.lens (\ResolveAppVersionResourcesResponse' {appArn} -> appArn) (\s@ResolveAppVersionResourcesResponse' {} a -> s {appArn = a} :: ResolveAppVersionResourcesResponse)

-- | The version of the application.
resolveAppVersionResourcesResponse_appVersion :: Lens.Lens' ResolveAppVersionResourcesResponse Prelude.Text
resolveAppVersionResourcesResponse_appVersion = Lens.lens (\ResolveAppVersionResourcesResponse' {appVersion} -> appVersion) (\s@ResolveAppVersionResourcesResponse' {} a -> s {appVersion = a} :: ResolveAppVersionResourcesResponse)

-- | The identifier for a specific resolution.
resolveAppVersionResourcesResponse_resolutionId :: Lens.Lens' ResolveAppVersionResourcesResponse Prelude.Text
resolveAppVersionResourcesResponse_resolutionId = Lens.lens (\ResolveAppVersionResourcesResponse' {resolutionId} -> resolutionId) (\s@ResolveAppVersionResourcesResponse' {} a -> s {resolutionId = a} :: ResolveAppVersionResourcesResponse)

-- | The status of the action.
resolveAppVersionResourcesResponse_status :: Lens.Lens' ResolveAppVersionResourcesResponse ResourceResolutionStatusType
resolveAppVersionResourcesResponse_status = Lens.lens (\ResolveAppVersionResourcesResponse' {status} -> status) (\s@ResolveAppVersionResourcesResponse' {} a -> s {status = a} :: ResolveAppVersionResourcesResponse)

instance
  Prelude.NFData
    ResolveAppVersionResourcesResponse
  where
  rnf ResolveAppVersionResourcesResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion
      `Prelude.seq` Prelude.rnf resolutionId
      `Prelude.seq` Prelude.rnf status

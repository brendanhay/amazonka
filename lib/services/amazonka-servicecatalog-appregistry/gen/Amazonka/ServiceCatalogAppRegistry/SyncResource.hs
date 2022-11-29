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
-- Module      : Amazonka.ServiceCatalogAppRegistry.SyncResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Syncs the resource with current AppRegistry records.
--
-- Specifically, the resource’s AppRegistry system tags sync with its
-- associated application. We remove the resource\'s AppRegistry system
-- tags if it does not associate with the application. The caller must have
-- permissions to read and update the resource.
module Amazonka.ServiceCatalogAppRegistry.SyncResource
  ( -- * Creating a Request
    SyncResource (..),
    newSyncResource,

    -- * Request Lenses
    syncResource_resourceType,
    syncResource_resource,

    -- * Destructuring the Response
    SyncResourceResponse (..),
    newSyncResourceResponse,

    -- * Response Lenses
    syncResourceResponse_applicationArn,
    syncResourceResponse_actionTaken,
    syncResourceResponse_resourceArn,
    syncResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalogAppRegistry.Types

-- | /See:/ 'newSyncResource' smart constructor.
data SyncResource = SyncResource'
  { -- | The type of resource of which the application will be associated.
    resourceType :: ResourceType,
    -- | An entity you can work with and specify with a name or ID. Examples
    -- include an Amazon EC2 instance, an Amazon Web Services CloudFormation
    -- stack, or an Amazon S3 bucket.
    resource :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SyncResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'syncResource_resourceType' - The type of resource of which the application will be associated.
--
-- 'resource', 'syncResource_resource' - An entity you can work with and specify with a name or ID. Examples
-- include an Amazon EC2 instance, an Amazon Web Services CloudFormation
-- stack, or an Amazon S3 bucket.
newSyncResource ::
  -- | 'resourceType'
  ResourceType ->
  -- | 'resource'
  Prelude.Text ->
  SyncResource
newSyncResource pResourceType_ pResource_ =
  SyncResource'
    { resourceType = pResourceType_,
      resource = pResource_
    }

-- | The type of resource of which the application will be associated.
syncResource_resourceType :: Lens.Lens' SyncResource ResourceType
syncResource_resourceType = Lens.lens (\SyncResource' {resourceType} -> resourceType) (\s@SyncResource' {} a -> s {resourceType = a} :: SyncResource)

-- | An entity you can work with and specify with a name or ID. Examples
-- include an Amazon EC2 instance, an Amazon Web Services CloudFormation
-- stack, or an Amazon S3 bucket.
syncResource_resource :: Lens.Lens' SyncResource Prelude.Text
syncResource_resource = Lens.lens (\SyncResource' {resource} -> resource) (\s@SyncResource' {} a -> s {resource = a} :: SyncResource)

instance Core.AWSRequest SyncResource where
  type AWSResponse SyncResource = SyncResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SyncResourceResponse'
            Prelude.<$> (x Core..?> "applicationArn")
            Prelude.<*> (x Core..?> "actionTaken")
            Prelude.<*> (x Core..?> "resourceArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SyncResource where
  hashWithSalt _salt SyncResource' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resource

instance Prelude.NFData SyncResource where
  rnf SyncResource' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resource

instance Core.ToHeaders SyncResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SyncResource where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath SyncResource where
  toPath SyncResource' {..} =
    Prelude.mconcat
      [ "/sync/",
        Core.toBS resourceType,
        "/",
        Core.toBS resource
      ]

instance Core.ToQuery SyncResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSyncResourceResponse' smart constructor.
data SyncResourceResponse = SyncResourceResponse'
  { -- | The Amazon resource name (ARN) that specifies the application.
    applicationArn :: Prelude.Maybe Prelude.Text,
    -- | The results of the output if an application is associated with an ARN
    -- value, which could be @syncStarted@ or None.
    actionTaken :: Prelude.Maybe SyncAction,
    -- | The Amazon resource name (ARN) that specifies the resource.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SyncResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationArn', 'syncResourceResponse_applicationArn' - The Amazon resource name (ARN) that specifies the application.
--
-- 'actionTaken', 'syncResourceResponse_actionTaken' - The results of the output if an application is associated with an ARN
-- value, which could be @syncStarted@ or None.
--
-- 'resourceArn', 'syncResourceResponse_resourceArn' - The Amazon resource name (ARN) that specifies the resource.
--
-- 'httpStatus', 'syncResourceResponse_httpStatus' - The response's http status code.
newSyncResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SyncResourceResponse
newSyncResourceResponse pHttpStatus_ =
  SyncResourceResponse'
    { applicationArn =
        Prelude.Nothing,
      actionTaken = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon resource name (ARN) that specifies the application.
syncResourceResponse_applicationArn :: Lens.Lens' SyncResourceResponse (Prelude.Maybe Prelude.Text)
syncResourceResponse_applicationArn = Lens.lens (\SyncResourceResponse' {applicationArn} -> applicationArn) (\s@SyncResourceResponse' {} a -> s {applicationArn = a} :: SyncResourceResponse)

-- | The results of the output if an application is associated with an ARN
-- value, which could be @syncStarted@ or None.
syncResourceResponse_actionTaken :: Lens.Lens' SyncResourceResponse (Prelude.Maybe SyncAction)
syncResourceResponse_actionTaken = Lens.lens (\SyncResourceResponse' {actionTaken} -> actionTaken) (\s@SyncResourceResponse' {} a -> s {actionTaken = a} :: SyncResourceResponse)

-- | The Amazon resource name (ARN) that specifies the resource.
syncResourceResponse_resourceArn :: Lens.Lens' SyncResourceResponse (Prelude.Maybe Prelude.Text)
syncResourceResponse_resourceArn = Lens.lens (\SyncResourceResponse' {resourceArn} -> resourceArn) (\s@SyncResourceResponse' {} a -> s {resourceArn = a} :: SyncResourceResponse)

-- | The response's http status code.
syncResourceResponse_httpStatus :: Lens.Lens' SyncResourceResponse Prelude.Int
syncResourceResponse_httpStatus = Lens.lens (\SyncResourceResponse' {httpStatus} -> httpStatus) (\s@SyncResourceResponse' {} a -> s {httpStatus = a} :: SyncResourceResponse)

instance Prelude.NFData SyncResourceResponse where
  rnf SyncResourceResponse' {..} =
    Prelude.rnf applicationArn
      `Prelude.seq` Prelude.rnf actionTaken
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf httpStatus

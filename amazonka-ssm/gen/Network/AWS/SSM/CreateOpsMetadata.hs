{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SSM.CreateOpsMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- If you create a new application in Application Manager, Systems Manager
-- calls this API action to specify information about the new application,
-- including the application type.
module Network.AWS.SSM.CreateOpsMetadata
  ( -- * Creating a Request
    CreateOpsMetadata (..),
    newCreateOpsMetadata,

    -- * Request Lenses
    createOpsMetadata_metadata,
    createOpsMetadata_resourceId,

    -- * Destructuring the Response
    CreateOpsMetadataResponse (..),
    newCreateOpsMetadataResponse,

    -- * Response Lenses
    createOpsMetadataResponse_opsMetadataArn,
    createOpsMetadataResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newCreateOpsMetadata' smart constructor.
data CreateOpsMetadata = CreateOpsMetadata'
  { -- | Metadata for a new Application Manager application.
    metadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text MetadataValue),
    -- | A resource ID for a new Application Manager application.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateOpsMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadata', 'createOpsMetadata_metadata' - Metadata for a new Application Manager application.
--
-- 'resourceId', 'createOpsMetadata_resourceId' - A resource ID for a new Application Manager application.
newCreateOpsMetadata ::
  -- | 'resourceId'
  Prelude.Text ->
  CreateOpsMetadata
newCreateOpsMetadata pResourceId_ =
  CreateOpsMetadata'
    { metadata = Prelude.Nothing,
      resourceId = pResourceId_
    }

-- | Metadata for a new Application Manager application.
createOpsMetadata_metadata :: Lens.Lens' CreateOpsMetadata (Prelude.Maybe (Prelude.HashMap Prelude.Text MetadataValue))
createOpsMetadata_metadata = Lens.lens (\CreateOpsMetadata' {metadata} -> metadata) (\s@CreateOpsMetadata' {} a -> s {metadata = a} :: CreateOpsMetadata) Prelude.. Lens.mapping Prelude._Coerce

-- | A resource ID for a new Application Manager application.
createOpsMetadata_resourceId :: Lens.Lens' CreateOpsMetadata Prelude.Text
createOpsMetadata_resourceId = Lens.lens (\CreateOpsMetadata' {resourceId} -> resourceId) (\s@CreateOpsMetadata' {} a -> s {resourceId = a} :: CreateOpsMetadata)

instance Prelude.AWSRequest CreateOpsMetadata where
  type Rs CreateOpsMetadata = CreateOpsMetadataResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOpsMetadataResponse'
            Prelude.<$> (x Prelude..?> "OpsMetadataArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateOpsMetadata

instance Prelude.NFData CreateOpsMetadata

instance Prelude.ToHeaders CreateOpsMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.CreateOpsMetadata" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateOpsMetadata where
  toJSON CreateOpsMetadata' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Metadata" Prelude..=) Prelude.<$> metadata,
            Prelude.Just ("ResourceId" Prelude..= resourceId)
          ]
      )

instance Prelude.ToPath CreateOpsMetadata where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateOpsMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateOpsMetadataResponse' smart constructor.
data CreateOpsMetadataResponse = CreateOpsMetadataResponse'
  { -- | The Amazon Resource Name (ARN) of the OpsMetadata Object or blob created
    -- by the call.
    opsMetadataArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateOpsMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'opsMetadataArn', 'createOpsMetadataResponse_opsMetadataArn' - The Amazon Resource Name (ARN) of the OpsMetadata Object or blob created
-- by the call.
--
-- 'httpStatus', 'createOpsMetadataResponse_httpStatus' - The response's http status code.
newCreateOpsMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateOpsMetadataResponse
newCreateOpsMetadataResponse pHttpStatus_ =
  CreateOpsMetadataResponse'
    { opsMetadataArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the OpsMetadata Object or blob created
-- by the call.
createOpsMetadataResponse_opsMetadataArn :: Lens.Lens' CreateOpsMetadataResponse (Prelude.Maybe Prelude.Text)
createOpsMetadataResponse_opsMetadataArn = Lens.lens (\CreateOpsMetadataResponse' {opsMetadataArn} -> opsMetadataArn) (\s@CreateOpsMetadataResponse' {} a -> s {opsMetadataArn = a} :: CreateOpsMetadataResponse)

-- | The response's http status code.
createOpsMetadataResponse_httpStatus :: Lens.Lens' CreateOpsMetadataResponse Prelude.Int
createOpsMetadataResponse_httpStatus = Lens.lens (\CreateOpsMetadataResponse' {httpStatus} -> httpStatus) (\s@CreateOpsMetadataResponse' {} a -> s {httpStatus = a} :: CreateOpsMetadataResponse)

instance Prelude.NFData CreateOpsMetadataResponse

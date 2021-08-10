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
-- Module      : Network.AWS.WorkDocs.CreateCustomMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more custom properties to the specified resource (a folder,
-- document, or version).
module Network.AWS.WorkDocs.CreateCustomMetadata
  ( -- * Creating a Request
    CreateCustomMetadata (..),
    newCreateCustomMetadata,

    -- * Request Lenses
    createCustomMetadata_versionId,
    createCustomMetadata_authenticationToken,
    createCustomMetadata_resourceId,
    createCustomMetadata_customMetadata,

    -- * Destructuring the Response
    CreateCustomMetadataResponse (..),
    newCreateCustomMetadataResponse,

    -- * Response Lenses
    createCustomMetadataResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newCreateCustomMetadata' smart constructor.
data CreateCustomMetadata = CreateCustomMetadata'
  { -- | The ID of the version, if the custom metadata is being added to a
    -- document version.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ID of the resource.
    resourceId :: Prelude.Text,
    -- | Custom metadata in the form of name-value pairs.
    customMetadata :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'createCustomMetadata_versionId' - The ID of the version, if the custom metadata is being added to a
-- document version.
--
-- 'authenticationToken', 'createCustomMetadata_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'resourceId', 'createCustomMetadata_resourceId' - The ID of the resource.
--
-- 'customMetadata', 'createCustomMetadata_customMetadata' - Custom metadata in the form of name-value pairs.
newCreateCustomMetadata ::
  -- | 'resourceId'
  Prelude.Text ->
  CreateCustomMetadata
newCreateCustomMetadata pResourceId_ =
  CreateCustomMetadata'
    { versionId = Prelude.Nothing,
      authenticationToken = Prelude.Nothing,
      resourceId = pResourceId_,
      customMetadata = Prelude.mempty
    }

-- | The ID of the version, if the custom metadata is being added to a
-- document version.
createCustomMetadata_versionId :: Lens.Lens' CreateCustomMetadata (Prelude.Maybe Prelude.Text)
createCustomMetadata_versionId = Lens.lens (\CreateCustomMetadata' {versionId} -> versionId) (\s@CreateCustomMetadata' {} a -> s {versionId = a} :: CreateCustomMetadata)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
createCustomMetadata_authenticationToken :: Lens.Lens' CreateCustomMetadata (Prelude.Maybe Prelude.Text)
createCustomMetadata_authenticationToken = Lens.lens (\CreateCustomMetadata' {authenticationToken} -> authenticationToken) (\s@CreateCustomMetadata' {} a -> s {authenticationToken = a} :: CreateCustomMetadata) Prelude.. Lens.mapping Core._Sensitive

-- | The ID of the resource.
createCustomMetadata_resourceId :: Lens.Lens' CreateCustomMetadata Prelude.Text
createCustomMetadata_resourceId = Lens.lens (\CreateCustomMetadata' {resourceId} -> resourceId) (\s@CreateCustomMetadata' {} a -> s {resourceId = a} :: CreateCustomMetadata)

-- | Custom metadata in the form of name-value pairs.
createCustomMetadata_customMetadata :: Lens.Lens' CreateCustomMetadata (Prelude.HashMap Prelude.Text Prelude.Text)
createCustomMetadata_customMetadata = Lens.lens (\CreateCustomMetadata' {customMetadata} -> customMetadata) (\s@CreateCustomMetadata' {} a -> s {customMetadata = a} :: CreateCustomMetadata) Prelude.. Lens._Coerce

instance Core.AWSRequest CreateCustomMetadata where
  type
    AWSResponse CreateCustomMetadata =
      CreateCustomMetadataResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateCustomMetadataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCustomMetadata

instance Prelude.NFData CreateCustomMetadata

instance Core.ToHeaders CreateCustomMetadata where
  toHeaders CreateCustomMetadata' {..} =
    Prelude.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON CreateCustomMetadata where
  toJSON CreateCustomMetadata' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CustomMetadata" Core..= customMetadata)
          ]
      )

instance Core.ToPath CreateCustomMetadata where
  toPath CreateCustomMetadata' {..} =
    Prelude.mconcat
      [ "/api/v1/resources/",
        Core.toBS resourceId,
        "/customMetadata"
      ]

instance Core.ToQuery CreateCustomMetadata where
  toQuery CreateCustomMetadata' {..} =
    Prelude.mconcat ["versionid" Core.=: versionId]

-- | /See:/ 'newCreateCustomMetadataResponse' smart constructor.
data CreateCustomMetadataResponse = CreateCustomMetadataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createCustomMetadataResponse_httpStatus' - The response's http status code.
newCreateCustomMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCustomMetadataResponse
newCreateCustomMetadataResponse pHttpStatus_ =
  CreateCustomMetadataResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createCustomMetadataResponse_httpStatus :: Lens.Lens' CreateCustomMetadataResponse Prelude.Int
createCustomMetadataResponse_httpStatus = Lens.lens (\CreateCustomMetadataResponse' {httpStatus} -> httpStatus) (\s@CreateCustomMetadataResponse' {} a -> s {httpStatus = a} :: CreateCustomMetadataResponse)

instance Prelude.NFData CreateCustomMetadataResponse

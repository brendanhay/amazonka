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
-- Module      : Amazonka.SSM.CreateOpsMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- If you create a new application in Application Manager, Amazon Web
-- Services Systems Manager calls this API operation to specify information
-- about the new application, including the application type.
module Amazonka.SSM.CreateOpsMetadata
  ( -- * Creating a Request
    CreateOpsMetadata (..),
    newCreateOpsMetadata,

    -- * Request Lenses
    createOpsMetadata_tags,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newCreateOpsMetadata' smart constructor.
data CreateOpsMetadata = CreateOpsMetadata'
  { -- | Optional metadata that you assign to a resource. You can specify a
    -- maximum of five tags for an OpsMetadata object. Tags enable you to
    -- categorize a resource in different ways, such as by purpose, owner, or
    -- environment. For example, you might want to tag an OpsMetadata object to
    -- identify an environment or target Amazon Web Services Region. In this
    -- case, you could specify the following key-value pairs:
    --
    -- -   @Key=Environment,Value=Production@
    --
    -- -   @Key=Region,Value=us-east-2@
    tags :: Prelude.Maybe [Tag],
    -- | Metadata for a new Application Manager application.
    metadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text MetadataValue),
    -- | A resource ID for a new Application Manager application.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOpsMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createOpsMetadata_tags' - Optional metadata that you assign to a resource. You can specify a
-- maximum of five tags for an OpsMetadata object. Tags enable you to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment. For example, you might want to tag an OpsMetadata object to
-- identify an environment or target Amazon Web Services Region. In this
-- case, you could specify the following key-value pairs:
--
-- -   @Key=Environment,Value=Production@
--
-- -   @Key=Region,Value=us-east-2@
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
    { tags = Prelude.Nothing,
      metadata = Prelude.Nothing,
      resourceId = pResourceId_
    }

-- | Optional metadata that you assign to a resource. You can specify a
-- maximum of five tags for an OpsMetadata object. Tags enable you to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment. For example, you might want to tag an OpsMetadata object to
-- identify an environment or target Amazon Web Services Region. In this
-- case, you could specify the following key-value pairs:
--
-- -   @Key=Environment,Value=Production@
--
-- -   @Key=Region,Value=us-east-2@
createOpsMetadata_tags :: Lens.Lens' CreateOpsMetadata (Prelude.Maybe [Tag])
createOpsMetadata_tags = Lens.lens (\CreateOpsMetadata' {tags} -> tags) (\s@CreateOpsMetadata' {} a -> s {tags = a} :: CreateOpsMetadata) Prelude.. Lens.mapping Lens.coerced

-- | Metadata for a new Application Manager application.
createOpsMetadata_metadata :: Lens.Lens' CreateOpsMetadata (Prelude.Maybe (Prelude.HashMap Prelude.Text MetadataValue))
createOpsMetadata_metadata = Lens.lens (\CreateOpsMetadata' {metadata} -> metadata) (\s@CreateOpsMetadata' {} a -> s {metadata = a} :: CreateOpsMetadata) Prelude.. Lens.mapping Lens.coerced

-- | A resource ID for a new Application Manager application.
createOpsMetadata_resourceId :: Lens.Lens' CreateOpsMetadata Prelude.Text
createOpsMetadata_resourceId = Lens.lens (\CreateOpsMetadata' {resourceId} -> resourceId) (\s@CreateOpsMetadata' {} a -> s {resourceId = a} :: CreateOpsMetadata)

instance Core.AWSRequest CreateOpsMetadata where
  type
    AWSResponse CreateOpsMetadata =
      CreateOpsMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOpsMetadataResponse'
            Prelude.<$> (x Data..?> "OpsMetadataArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateOpsMetadata where
  hashWithSalt _salt CreateOpsMetadata' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData CreateOpsMetadata where
  rnf CreateOpsMetadata' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf resourceId

instance Data.ToHeaders CreateOpsMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.CreateOpsMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateOpsMetadata where
  toJSON CreateOpsMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("Metadata" Data..=) Prelude.<$> metadata,
            Prelude.Just ("ResourceId" Data..= resourceId)
          ]
      )

instance Data.ToPath CreateOpsMetadata where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateOpsMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateOpsMetadataResponse' smart constructor.
data CreateOpsMetadataResponse = CreateOpsMetadataResponse'
  { -- | The Amazon Resource Name (ARN) of the OpsMetadata Object or blob created
    -- by the call.
    opsMetadataArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateOpsMetadataResponse where
  rnf CreateOpsMetadataResponse' {..} =
    Prelude.rnf opsMetadataArn
      `Prelude.seq` Prelude.rnf httpStatus

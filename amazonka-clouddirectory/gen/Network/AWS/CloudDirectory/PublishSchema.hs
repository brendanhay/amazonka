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
-- Module      : Network.AWS.CloudDirectory.PublishSchema
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Publishes a development schema with a major version and a recommended
-- minor version.
module Network.AWS.CloudDirectory.PublishSchema
  ( -- * Creating a Request
    PublishSchema (..),
    newPublishSchema,

    -- * Request Lenses
    publishSchema_name,
    publishSchema_minorVersion,
    publishSchema_developmentSchemaArn,
    publishSchema_version,

    -- * Destructuring the Response
    PublishSchemaResponse (..),
    newPublishSchemaResponse,

    -- * Response Lenses
    publishSchemaResponse_publishedSchemaArn,
    publishSchemaResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPublishSchema' smart constructor.
data PublishSchema = PublishSchema'
  { -- | The new name under which the schema will be published. If this is not
    -- provided, the development schema is considered.
    name :: Core.Maybe Core.Text,
    -- | The minor version under which the schema will be published. This
    -- parameter is recommended. Schemas have both a major and minor version
    -- associated with them.
    minorVersion :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) that is associated with the development
    -- schema. For more information, see arns.
    developmentSchemaArn :: Core.Text,
    -- | The major version under which the schema will be published. Schemas have
    -- both a major and minor version associated with them.
    version :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PublishSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'publishSchema_name' - The new name under which the schema will be published. If this is not
-- provided, the development schema is considered.
--
-- 'minorVersion', 'publishSchema_minorVersion' - The minor version under which the schema will be published. This
-- parameter is recommended. Schemas have both a major and minor version
-- associated with them.
--
-- 'developmentSchemaArn', 'publishSchema_developmentSchemaArn' - The Amazon Resource Name (ARN) that is associated with the development
-- schema. For more information, see arns.
--
-- 'version', 'publishSchema_version' - The major version under which the schema will be published. Schemas have
-- both a major and minor version associated with them.
newPublishSchema ::
  -- | 'developmentSchemaArn'
  Core.Text ->
  -- | 'version'
  Core.Text ->
  PublishSchema
newPublishSchema pDevelopmentSchemaArn_ pVersion_ =
  PublishSchema'
    { name = Core.Nothing,
      minorVersion = Core.Nothing,
      developmentSchemaArn = pDevelopmentSchemaArn_,
      version = pVersion_
    }

-- | The new name under which the schema will be published. If this is not
-- provided, the development schema is considered.
publishSchema_name :: Lens.Lens' PublishSchema (Core.Maybe Core.Text)
publishSchema_name = Lens.lens (\PublishSchema' {name} -> name) (\s@PublishSchema' {} a -> s {name = a} :: PublishSchema)

-- | The minor version under which the schema will be published. This
-- parameter is recommended. Schemas have both a major and minor version
-- associated with them.
publishSchema_minorVersion :: Lens.Lens' PublishSchema (Core.Maybe Core.Text)
publishSchema_minorVersion = Lens.lens (\PublishSchema' {minorVersion} -> minorVersion) (\s@PublishSchema' {} a -> s {minorVersion = a} :: PublishSchema)

-- | The Amazon Resource Name (ARN) that is associated with the development
-- schema. For more information, see arns.
publishSchema_developmentSchemaArn :: Lens.Lens' PublishSchema Core.Text
publishSchema_developmentSchemaArn = Lens.lens (\PublishSchema' {developmentSchemaArn} -> developmentSchemaArn) (\s@PublishSchema' {} a -> s {developmentSchemaArn = a} :: PublishSchema)

-- | The major version under which the schema will be published. Schemas have
-- both a major and minor version associated with them.
publishSchema_version :: Lens.Lens' PublishSchema Core.Text
publishSchema_version = Lens.lens (\PublishSchema' {version} -> version) (\s@PublishSchema' {} a -> s {version = a} :: PublishSchema)

instance Core.AWSRequest PublishSchema where
  type
    AWSResponse PublishSchema =
      PublishSchemaResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PublishSchemaResponse'
            Core.<$> (x Core..?> "PublishedSchemaArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PublishSchema

instance Core.NFData PublishSchema

instance Core.ToHeaders PublishSchema where
  toHeaders PublishSchema' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# developmentSchemaArn]

instance Core.ToJSON PublishSchema where
  toJSON PublishSchema' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("MinorVersion" Core..=) Core.<$> minorVersion,
            Core.Just ("Version" Core..= version)
          ]
      )

instance Core.ToPath PublishSchema where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/schema/publish"

instance Core.ToQuery PublishSchema where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPublishSchemaResponse' smart constructor.
data PublishSchemaResponse = PublishSchemaResponse'
  { -- | The ARN that is associated with the published schema. For more
    -- information, see arns.
    publishedSchemaArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PublishSchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publishedSchemaArn', 'publishSchemaResponse_publishedSchemaArn' - The ARN that is associated with the published schema. For more
-- information, see arns.
--
-- 'httpStatus', 'publishSchemaResponse_httpStatus' - The response's http status code.
newPublishSchemaResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PublishSchemaResponse
newPublishSchemaResponse pHttpStatus_ =
  PublishSchemaResponse'
    { publishedSchemaArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN that is associated with the published schema. For more
-- information, see arns.
publishSchemaResponse_publishedSchemaArn :: Lens.Lens' PublishSchemaResponse (Core.Maybe Core.Text)
publishSchemaResponse_publishedSchemaArn = Lens.lens (\PublishSchemaResponse' {publishedSchemaArn} -> publishedSchemaArn) (\s@PublishSchemaResponse' {} a -> s {publishedSchemaArn = a} :: PublishSchemaResponse)

-- | The response's http status code.
publishSchemaResponse_httpStatus :: Lens.Lens' PublishSchemaResponse Core.Int
publishSchemaResponse_httpStatus = Lens.lens (\PublishSchemaResponse' {httpStatus} -> httpStatus) (\s@PublishSchemaResponse' {} a -> s {httpStatus = a} :: PublishSchemaResponse)

instance Core.NFData PublishSchemaResponse

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
-- Module      : Network.AWS.CloudDirectory.ApplySchema
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the input published schema, at the specified version, into the
-- Directory with the same name and version as that of the published
-- schema.
module Network.AWS.CloudDirectory.ApplySchema
  ( -- * Creating a Request
    ApplySchema (..),
    newApplySchema,

    -- * Request Lenses
    applySchema_publishedSchemaArn,
    applySchema_directoryArn,

    -- * Destructuring the Response
    ApplySchemaResponse (..),
    newApplySchemaResponse,

    -- * Response Lenses
    applySchemaResponse_directoryArn,
    applySchemaResponse_appliedSchemaArn,
    applySchemaResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newApplySchema' smart constructor.
data ApplySchema = ApplySchema'
  { -- | Published schema Amazon Resource Name (ARN) that needs to be copied. For
    -- more information, see arns.
    publishedSchemaArn :: Core.Text,
    -- | The Amazon Resource Name (ARN) that is associated with the Directory
    -- into which the schema is copied. For more information, see arns.
    directoryArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApplySchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publishedSchemaArn', 'applySchema_publishedSchemaArn' - Published schema Amazon Resource Name (ARN) that needs to be copied. For
-- more information, see arns.
--
-- 'directoryArn', 'applySchema_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory
-- into which the schema is copied. For more information, see arns.
newApplySchema ::
  -- | 'publishedSchemaArn'
  Core.Text ->
  -- | 'directoryArn'
  Core.Text ->
  ApplySchema
newApplySchema pPublishedSchemaArn_ pDirectoryArn_ =
  ApplySchema'
    { publishedSchemaArn =
        pPublishedSchemaArn_,
      directoryArn = pDirectoryArn_
    }

-- | Published schema Amazon Resource Name (ARN) that needs to be copied. For
-- more information, see arns.
applySchema_publishedSchemaArn :: Lens.Lens' ApplySchema Core.Text
applySchema_publishedSchemaArn = Lens.lens (\ApplySchema' {publishedSchemaArn} -> publishedSchemaArn) (\s@ApplySchema' {} a -> s {publishedSchemaArn = a} :: ApplySchema)

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- into which the schema is copied. For more information, see arns.
applySchema_directoryArn :: Lens.Lens' ApplySchema Core.Text
applySchema_directoryArn = Lens.lens (\ApplySchema' {directoryArn} -> directoryArn) (\s@ApplySchema' {} a -> s {directoryArn = a} :: ApplySchema)

instance Core.AWSRequest ApplySchema where
  type AWSResponse ApplySchema = ApplySchemaResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ApplySchemaResponse'
            Core.<$> (x Core..?> "DirectoryArn")
            Core.<*> (x Core..?> "AppliedSchemaArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ApplySchema

instance Core.NFData ApplySchema

instance Core.ToHeaders ApplySchema where
  toHeaders ApplySchema' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON ApplySchema where
  toJSON ApplySchema' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("PublishedSchemaArn" Core..= publishedSchemaArn)
          ]
      )

instance Core.ToPath ApplySchema where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/schema/apply"

instance Core.ToQuery ApplySchema where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newApplySchemaResponse' smart constructor.
data ApplySchemaResponse = ApplySchemaResponse'
  { -- | The ARN that is associated with the Directory. For more information, see
    -- arns.
    directoryArn :: Core.Maybe Core.Text,
    -- | The applied schema ARN that is associated with the copied schema in the
    -- Directory. You can use this ARN to describe the schema information
    -- applied on this directory. For more information, see arns.
    appliedSchemaArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApplySchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryArn', 'applySchemaResponse_directoryArn' - The ARN that is associated with the Directory. For more information, see
-- arns.
--
-- 'appliedSchemaArn', 'applySchemaResponse_appliedSchemaArn' - The applied schema ARN that is associated with the copied schema in the
-- Directory. You can use this ARN to describe the schema information
-- applied on this directory. For more information, see arns.
--
-- 'httpStatus', 'applySchemaResponse_httpStatus' - The response's http status code.
newApplySchemaResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ApplySchemaResponse
newApplySchemaResponse pHttpStatus_ =
  ApplySchemaResponse'
    { directoryArn = Core.Nothing,
      appliedSchemaArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN that is associated with the Directory. For more information, see
-- arns.
applySchemaResponse_directoryArn :: Lens.Lens' ApplySchemaResponse (Core.Maybe Core.Text)
applySchemaResponse_directoryArn = Lens.lens (\ApplySchemaResponse' {directoryArn} -> directoryArn) (\s@ApplySchemaResponse' {} a -> s {directoryArn = a} :: ApplySchemaResponse)

-- | The applied schema ARN that is associated with the copied schema in the
-- Directory. You can use this ARN to describe the schema information
-- applied on this directory. For more information, see arns.
applySchemaResponse_appliedSchemaArn :: Lens.Lens' ApplySchemaResponse (Core.Maybe Core.Text)
applySchemaResponse_appliedSchemaArn = Lens.lens (\ApplySchemaResponse' {appliedSchemaArn} -> appliedSchemaArn) (\s@ApplySchemaResponse' {} a -> s {appliedSchemaArn = a} :: ApplySchemaResponse)

-- | The response's http status code.
applySchemaResponse_httpStatus :: Lens.Lens' ApplySchemaResponse Core.Int
applySchemaResponse_httpStatus = Lens.lens (\ApplySchemaResponse' {httpStatus} -> httpStatus) (\s@ApplySchemaResponse' {} a -> s {httpStatus = a} :: ApplySchemaResponse)

instance Core.NFData ApplySchemaResponse

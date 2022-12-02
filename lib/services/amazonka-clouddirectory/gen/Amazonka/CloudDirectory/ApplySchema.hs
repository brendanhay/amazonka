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
-- Module      : Amazonka.CloudDirectory.ApplySchema
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the input published schema, at the specified version, into the
-- Directory with the same name and version as that of the published
-- schema.
module Amazonka.CloudDirectory.ApplySchema
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

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newApplySchema' smart constructor.
data ApplySchema = ApplySchema'
  { -- | Published schema Amazon Resource Name (ARN) that needs to be copied. For
    -- more information, see arns.
    publishedSchemaArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) that is associated with the Directory
    -- into which the schema is copied. For more information, see arns.
    directoryArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'directoryArn'
  Prelude.Text ->
  ApplySchema
newApplySchema pPublishedSchemaArn_ pDirectoryArn_ =
  ApplySchema'
    { publishedSchemaArn =
        pPublishedSchemaArn_,
      directoryArn = pDirectoryArn_
    }

-- | Published schema Amazon Resource Name (ARN) that needs to be copied. For
-- more information, see arns.
applySchema_publishedSchemaArn :: Lens.Lens' ApplySchema Prelude.Text
applySchema_publishedSchemaArn = Lens.lens (\ApplySchema' {publishedSchemaArn} -> publishedSchemaArn) (\s@ApplySchema' {} a -> s {publishedSchemaArn = a} :: ApplySchema)

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- into which the schema is copied. For more information, see arns.
applySchema_directoryArn :: Lens.Lens' ApplySchema Prelude.Text
applySchema_directoryArn = Lens.lens (\ApplySchema' {directoryArn} -> directoryArn) (\s@ApplySchema' {} a -> s {directoryArn = a} :: ApplySchema)

instance Core.AWSRequest ApplySchema where
  type AWSResponse ApplySchema = ApplySchemaResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ApplySchemaResponse'
            Prelude.<$> (x Data..?> "DirectoryArn")
            Prelude.<*> (x Data..?> "AppliedSchemaArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ApplySchema where
  hashWithSalt _salt ApplySchema' {..} =
    _salt `Prelude.hashWithSalt` publishedSchemaArn
      `Prelude.hashWithSalt` directoryArn

instance Prelude.NFData ApplySchema where
  rnf ApplySchema' {..} =
    Prelude.rnf publishedSchemaArn
      `Prelude.seq` Prelude.rnf directoryArn

instance Data.ToHeaders ApplySchema where
  toHeaders ApplySchema' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Data.=# directoryArn]

instance Data.ToJSON ApplySchema where
  toJSON ApplySchema' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PublishedSchemaArn" Data..= publishedSchemaArn)
          ]
      )

instance Data.ToPath ApplySchema where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/schema/apply"

instance Data.ToQuery ApplySchema where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newApplySchemaResponse' smart constructor.
data ApplySchemaResponse = ApplySchemaResponse'
  { -- | The ARN that is associated with the Directory. For more information, see
    -- arns.
    directoryArn :: Prelude.Maybe Prelude.Text,
    -- | The applied schema ARN that is associated with the copied schema in the
    -- Directory. You can use this ARN to describe the schema information
    -- applied on this directory. For more information, see arns.
    appliedSchemaArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ApplySchemaResponse
newApplySchemaResponse pHttpStatus_ =
  ApplySchemaResponse'
    { directoryArn =
        Prelude.Nothing,
      appliedSchemaArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN that is associated with the Directory. For more information, see
-- arns.
applySchemaResponse_directoryArn :: Lens.Lens' ApplySchemaResponse (Prelude.Maybe Prelude.Text)
applySchemaResponse_directoryArn = Lens.lens (\ApplySchemaResponse' {directoryArn} -> directoryArn) (\s@ApplySchemaResponse' {} a -> s {directoryArn = a} :: ApplySchemaResponse)

-- | The applied schema ARN that is associated with the copied schema in the
-- Directory. You can use this ARN to describe the schema information
-- applied on this directory. For more information, see arns.
applySchemaResponse_appliedSchemaArn :: Lens.Lens' ApplySchemaResponse (Prelude.Maybe Prelude.Text)
applySchemaResponse_appliedSchemaArn = Lens.lens (\ApplySchemaResponse' {appliedSchemaArn} -> appliedSchemaArn) (\s@ApplySchemaResponse' {} a -> s {appliedSchemaArn = a} :: ApplySchemaResponse)

-- | The response's http status code.
applySchemaResponse_httpStatus :: Lens.Lens' ApplySchemaResponse Prelude.Int
applySchemaResponse_httpStatus = Lens.lens (\ApplySchemaResponse' {httpStatus} -> httpStatus) (\s@ApplySchemaResponse' {} a -> s {httpStatus = a} :: ApplySchemaResponse)

instance Prelude.NFData ApplySchemaResponse where
  rnf ApplySchemaResponse' {..} =
    Prelude.rnf directoryArn
      `Prelude.seq` Prelude.rnf appliedSchemaArn
      `Prelude.seq` Prelude.rnf httpStatus

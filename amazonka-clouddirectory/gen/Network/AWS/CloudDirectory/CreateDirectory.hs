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
-- Module      : Network.AWS.CloudDirectory.CreateDirectory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Directory by copying the published schema into the directory.
-- A directory cannot be created without a schema.
--
-- You can also quickly create a directory using a managed schema, called
-- the @QuickStartSchema@. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_managed.html Managed Schema>
-- in the /Amazon Cloud Directory Developer Guide/.
module Network.AWS.CloudDirectory.CreateDirectory
  ( -- * Creating a Request
    CreateDirectory (..),
    newCreateDirectory,

    -- * Request Lenses
    createDirectory_name,
    createDirectory_schemaArn,

    -- * Destructuring the Response
    CreateDirectoryResponse (..),
    newCreateDirectoryResponse,

    -- * Response Lenses
    createDirectoryResponse_httpStatus,
    createDirectoryResponse_directoryArn,
    createDirectoryResponse_name,
    createDirectoryResponse_objectIdentifier,
    createDirectoryResponse_appliedSchemaArn,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDirectory' smart constructor.
data CreateDirectory = CreateDirectory'
  { -- | The name of the Directory. Should be unique per account, per region.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the published schema that will be
    -- copied into the data Directory. For more information, see arns.
    schemaArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDirectory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createDirectory_name' - The name of the Directory. Should be unique per account, per region.
--
-- 'schemaArn', 'createDirectory_schemaArn' - The Amazon Resource Name (ARN) of the published schema that will be
-- copied into the data Directory. For more information, see arns.
newCreateDirectory ::
  -- | 'name'
  Prelude.Text ->
  -- | 'schemaArn'
  Prelude.Text ->
  CreateDirectory
newCreateDirectory pName_ pSchemaArn_ =
  CreateDirectory'
    { name = pName_,
      schemaArn = pSchemaArn_
    }

-- | The name of the Directory. Should be unique per account, per region.
createDirectory_name :: Lens.Lens' CreateDirectory Prelude.Text
createDirectory_name = Lens.lens (\CreateDirectory' {name} -> name) (\s@CreateDirectory' {} a -> s {name = a} :: CreateDirectory)

-- | The Amazon Resource Name (ARN) of the published schema that will be
-- copied into the data Directory. For more information, see arns.
createDirectory_schemaArn :: Lens.Lens' CreateDirectory Prelude.Text
createDirectory_schemaArn = Lens.lens (\CreateDirectory' {schemaArn} -> schemaArn) (\s@CreateDirectory' {} a -> s {schemaArn = a} :: CreateDirectory)

instance Prelude.AWSRequest CreateDirectory where
  type Rs CreateDirectory = CreateDirectoryResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDirectoryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "DirectoryArn")
            Prelude.<*> (x Prelude..:> "Name")
            Prelude.<*> (x Prelude..:> "ObjectIdentifier")
            Prelude.<*> (x Prelude..:> "AppliedSchemaArn")
      )

instance Prelude.Hashable CreateDirectory

instance Prelude.NFData CreateDirectory

instance Prelude.ToHeaders CreateDirectory where
  toHeaders CreateDirectory' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Prelude.=# schemaArn]

instance Prelude.ToJSON CreateDirectory where
  toJSON CreateDirectory' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath CreateDirectory where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/directory/create"

instance Prelude.ToQuery CreateDirectory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDirectoryResponse' smart constructor.
data CreateDirectoryResponse = CreateDirectoryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN that is associated with the Directory. For more information, see
    -- arns.
    directoryArn :: Prelude.Text,
    -- | The name of the Directory.
    name :: Prelude.Text,
    -- | The root object node of the created directory.
    objectIdentifier :: Prelude.Text,
    -- | The ARN of the published schema in the Directory. Once a published
    -- schema is copied into the directory, it has its own ARN, which is
    -- referred to applied schema ARN. For more information, see arns.
    appliedSchemaArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDirectoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createDirectoryResponse_httpStatus' - The response's http status code.
--
-- 'directoryArn', 'createDirectoryResponse_directoryArn' - The ARN that is associated with the Directory. For more information, see
-- arns.
--
-- 'name', 'createDirectoryResponse_name' - The name of the Directory.
--
-- 'objectIdentifier', 'createDirectoryResponse_objectIdentifier' - The root object node of the created directory.
--
-- 'appliedSchemaArn', 'createDirectoryResponse_appliedSchemaArn' - The ARN of the published schema in the Directory. Once a published
-- schema is copied into the directory, it has its own ARN, which is
-- referred to applied schema ARN. For more information, see arns.
newCreateDirectoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'directoryArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'objectIdentifier'
  Prelude.Text ->
  -- | 'appliedSchemaArn'
  Prelude.Text ->
  CreateDirectoryResponse
newCreateDirectoryResponse
  pHttpStatus_
  pDirectoryArn_
  pName_
  pObjectIdentifier_
  pAppliedSchemaArn_ =
    CreateDirectoryResponse'
      { httpStatus = pHttpStatus_,
        directoryArn = pDirectoryArn_,
        name = pName_,
        objectIdentifier = pObjectIdentifier_,
        appliedSchemaArn = pAppliedSchemaArn_
      }

-- | The response's http status code.
createDirectoryResponse_httpStatus :: Lens.Lens' CreateDirectoryResponse Prelude.Int
createDirectoryResponse_httpStatus = Lens.lens (\CreateDirectoryResponse' {httpStatus} -> httpStatus) (\s@CreateDirectoryResponse' {} a -> s {httpStatus = a} :: CreateDirectoryResponse)

-- | The ARN that is associated with the Directory. For more information, see
-- arns.
createDirectoryResponse_directoryArn :: Lens.Lens' CreateDirectoryResponse Prelude.Text
createDirectoryResponse_directoryArn = Lens.lens (\CreateDirectoryResponse' {directoryArn} -> directoryArn) (\s@CreateDirectoryResponse' {} a -> s {directoryArn = a} :: CreateDirectoryResponse)

-- | The name of the Directory.
createDirectoryResponse_name :: Lens.Lens' CreateDirectoryResponse Prelude.Text
createDirectoryResponse_name = Lens.lens (\CreateDirectoryResponse' {name} -> name) (\s@CreateDirectoryResponse' {} a -> s {name = a} :: CreateDirectoryResponse)

-- | The root object node of the created directory.
createDirectoryResponse_objectIdentifier :: Lens.Lens' CreateDirectoryResponse Prelude.Text
createDirectoryResponse_objectIdentifier = Lens.lens (\CreateDirectoryResponse' {objectIdentifier} -> objectIdentifier) (\s@CreateDirectoryResponse' {} a -> s {objectIdentifier = a} :: CreateDirectoryResponse)

-- | The ARN of the published schema in the Directory. Once a published
-- schema is copied into the directory, it has its own ARN, which is
-- referred to applied schema ARN. For more information, see arns.
createDirectoryResponse_appliedSchemaArn :: Lens.Lens' CreateDirectoryResponse Prelude.Text
createDirectoryResponse_appliedSchemaArn = Lens.lens (\CreateDirectoryResponse' {appliedSchemaArn} -> appliedSchemaArn) (\s@CreateDirectoryResponse' {} a -> s {appliedSchemaArn = a} :: CreateDirectoryResponse)

instance Prelude.NFData CreateDirectoryResponse

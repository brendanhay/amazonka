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
-- Module      : Amazonka.DirectoryService.CreateAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an alias for a directory and assigns the alias to the directory.
-- The alias is used to construct the access URL for the directory, such as
-- @http:\/\/\<alias>.awsapps.com@.
--
-- After an alias has been created, it cannot be deleted or reused, so this
-- operation should only be used when absolutely necessary.
module Amazonka.DirectoryService.CreateAlias
  ( -- * Creating a Request
    CreateAlias (..),
    newCreateAlias,

    -- * Request Lenses
    createAlias_directoryId,
    createAlias_alias,

    -- * Destructuring the Response
    CreateAliasResponse (..),
    newCreateAliasResponse,

    -- * Response Lenses
    createAliasResponse_alias,
    createAliasResponse_directoryId,
    createAliasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the inputs for the CreateAlias operation.
--
-- /See:/ 'newCreateAlias' smart constructor.
data CreateAlias = CreateAlias'
  { -- | The identifier of the directory for which to create the alias.
    directoryId :: Prelude.Text,
    -- | The requested alias.
    --
    -- The alias must be unique amongst all aliases in Amazon Web Services.
    -- This operation throws an @EntityAlreadyExistsException@ error if the
    -- alias already exists.
    alias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'createAlias_directoryId' - The identifier of the directory for which to create the alias.
--
-- 'alias', 'createAlias_alias' - The requested alias.
--
-- The alias must be unique amongst all aliases in Amazon Web Services.
-- This operation throws an @EntityAlreadyExistsException@ error if the
-- alias already exists.
newCreateAlias ::
  -- | 'directoryId'
  Prelude.Text ->
  -- | 'alias'
  Prelude.Text ->
  CreateAlias
newCreateAlias pDirectoryId_ pAlias_ =
  CreateAlias'
    { directoryId = pDirectoryId_,
      alias = pAlias_
    }

-- | The identifier of the directory for which to create the alias.
createAlias_directoryId :: Lens.Lens' CreateAlias Prelude.Text
createAlias_directoryId = Lens.lens (\CreateAlias' {directoryId} -> directoryId) (\s@CreateAlias' {} a -> s {directoryId = a} :: CreateAlias)

-- | The requested alias.
--
-- The alias must be unique amongst all aliases in Amazon Web Services.
-- This operation throws an @EntityAlreadyExistsException@ error if the
-- alias already exists.
createAlias_alias :: Lens.Lens' CreateAlias Prelude.Text
createAlias_alias = Lens.lens (\CreateAlias' {alias} -> alias) (\s@CreateAlias' {} a -> s {alias = a} :: CreateAlias)

instance Core.AWSRequest CreateAlias where
  type AWSResponse CreateAlias = CreateAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAliasResponse'
            Prelude.<$> (x Data..?> "Alias")
            Prelude.<*> (x Data..?> "DirectoryId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAlias where
  hashWithSalt _salt CreateAlias' {..} =
    _salt
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` alias

instance Prelude.NFData CreateAlias where
  rnf CreateAlias' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf alias

instance Data.ToHeaders CreateAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.CreateAlias" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAlias where
  toJSON CreateAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Data..= directoryId),
            Prelude.Just ("Alias" Data..= alias)
          ]
      )

instance Data.ToPath CreateAlias where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateAlias where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the results of the CreateAlias operation.
--
-- /See:/ 'newCreateAliasResponse' smart constructor.
data CreateAliasResponse = CreateAliasResponse'
  { -- | The alias for the directory.
    alias :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the directory.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'createAliasResponse_alias' - The alias for the directory.
--
-- 'directoryId', 'createAliasResponse_directoryId' - The identifier of the directory.
--
-- 'httpStatus', 'createAliasResponse_httpStatus' - The response's http status code.
newCreateAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAliasResponse
newCreateAliasResponse pHttpStatus_ =
  CreateAliasResponse'
    { alias = Prelude.Nothing,
      directoryId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The alias for the directory.
createAliasResponse_alias :: Lens.Lens' CreateAliasResponse (Prelude.Maybe Prelude.Text)
createAliasResponse_alias = Lens.lens (\CreateAliasResponse' {alias} -> alias) (\s@CreateAliasResponse' {} a -> s {alias = a} :: CreateAliasResponse)

-- | The identifier of the directory.
createAliasResponse_directoryId :: Lens.Lens' CreateAliasResponse (Prelude.Maybe Prelude.Text)
createAliasResponse_directoryId = Lens.lens (\CreateAliasResponse' {directoryId} -> directoryId) (\s@CreateAliasResponse' {} a -> s {directoryId = a} :: CreateAliasResponse)

-- | The response's http status code.
createAliasResponse_httpStatus :: Lens.Lens' CreateAliasResponse Prelude.Int
createAliasResponse_httpStatus = Lens.lens (\CreateAliasResponse' {httpStatus} -> httpStatus) (\s@CreateAliasResponse' {} a -> s {httpStatus = a} :: CreateAliasResponse)

instance Prelude.NFData CreateAliasResponse where
  rnf CreateAliasResponse' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Network.AWS.WorkSpaces.CreateConnectionAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the specified connection alias for use with cross-Region
-- redirection. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces>.
module Network.AWS.WorkSpaces.CreateConnectionAlias
  ( -- * Creating a Request
    CreateConnectionAlias (..),
    newCreateConnectionAlias,

    -- * Request Lenses
    createConnectionAlias_tags,
    createConnectionAlias_connectionString,

    -- * Destructuring the Response
    CreateConnectionAliasResponse (..),
    newCreateConnectionAliasResponse,

    -- * Response Lenses
    createConnectionAliasResponse_aliasId,
    createConnectionAliasResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newCreateConnectionAlias' smart constructor.
data CreateConnectionAlias = CreateConnectionAlias'
  { -- | The tags to associate with the connection alias.
    tags :: Prelude.Maybe [Tag],
    -- | A connection string in the form of a fully qualified domain name (FQDN),
    -- such as @www.example.com@.
    --
    -- After you create a connection string, it is always associated to your
    -- AWS account. You cannot recreate the same connection string with a
    -- different account, even if you delete all instances of it from the
    -- original account. The connection string is globally reserved for your
    -- account.
    connectionString :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateConnectionAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createConnectionAlias_tags' - The tags to associate with the connection alias.
--
-- 'connectionString', 'createConnectionAlias_connectionString' - A connection string in the form of a fully qualified domain name (FQDN),
-- such as @www.example.com@.
--
-- After you create a connection string, it is always associated to your
-- AWS account. You cannot recreate the same connection string with a
-- different account, even if you delete all instances of it from the
-- original account. The connection string is globally reserved for your
-- account.
newCreateConnectionAlias ::
  -- | 'connectionString'
  Prelude.Text ->
  CreateConnectionAlias
newCreateConnectionAlias pConnectionString_ =
  CreateConnectionAlias'
    { tags = Prelude.Nothing,
      connectionString = pConnectionString_
    }

-- | The tags to associate with the connection alias.
createConnectionAlias_tags :: Lens.Lens' CreateConnectionAlias (Prelude.Maybe [Tag])
createConnectionAlias_tags = Lens.lens (\CreateConnectionAlias' {tags} -> tags) (\s@CreateConnectionAlias' {} a -> s {tags = a} :: CreateConnectionAlias) Prelude.. Lens.mapping Prelude._Coerce

-- | A connection string in the form of a fully qualified domain name (FQDN),
-- such as @www.example.com@.
--
-- After you create a connection string, it is always associated to your
-- AWS account. You cannot recreate the same connection string with a
-- different account, even if you delete all instances of it from the
-- original account. The connection string is globally reserved for your
-- account.
createConnectionAlias_connectionString :: Lens.Lens' CreateConnectionAlias Prelude.Text
createConnectionAlias_connectionString = Lens.lens (\CreateConnectionAlias' {connectionString} -> connectionString) (\s@CreateConnectionAlias' {} a -> s {connectionString = a} :: CreateConnectionAlias)

instance Prelude.AWSRequest CreateConnectionAlias where
  type
    Rs CreateConnectionAlias =
      CreateConnectionAliasResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConnectionAliasResponse'
            Prelude.<$> (x Prelude..?> "AliasId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateConnectionAlias

instance Prelude.NFData CreateConnectionAlias

instance Prelude.ToHeaders CreateConnectionAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkspacesService.CreateConnectionAlias" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateConnectionAlias where
  toJSON CreateConnectionAlias' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Tags" Prelude..=) Prelude.<$> tags,
            Prelude.Just
              ("ConnectionString" Prelude..= connectionString)
          ]
      )

instance Prelude.ToPath CreateConnectionAlias where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateConnectionAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConnectionAliasResponse' smart constructor.
data CreateConnectionAliasResponse = CreateConnectionAliasResponse'
  { -- | The identifier of the connection alias.
    aliasId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateConnectionAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasId', 'createConnectionAliasResponse_aliasId' - The identifier of the connection alias.
--
-- 'httpStatus', 'createConnectionAliasResponse_httpStatus' - The response's http status code.
newCreateConnectionAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateConnectionAliasResponse
newCreateConnectionAliasResponse pHttpStatus_ =
  CreateConnectionAliasResponse'
    { aliasId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the connection alias.
createConnectionAliasResponse_aliasId :: Lens.Lens' CreateConnectionAliasResponse (Prelude.Maybe Prelude.Text)
createConnectionAliasResponse_aliasId = Lens.lens (\CreateConnectionAliasResponse' {aliasId} -> aliasId) (\s@CreateConnectionAliasResponse' {} a -> s {aliasId = a} :: CreateConnectionAliasResponse)

-- | The response's http status code.
createConnectionAliasResponse_httpStatus :: Lens.Lens' CreateConnectionAliasResponse Prelude.Int
createConnectionAliasResponse_httpStatus = Lens.lens (\CreateConnectionAliasResponse' {httpStatus} -> httpStatus) (\s@CreateConnectionAliasResponse' {} a -> s {httpStatus = a} :: CreateConnectionAliasResponse)

instance Prelude.NFData CreateConnectionAliasResponse

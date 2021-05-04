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
-- Module      : Network.AWS.WorkSpaces.AssociateConnectionAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified connection alias with the specified directory
-- to enable cross-Region redirection. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces>.
--
-- Before performing this operation, call
-- <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeConnectionAliases.html DescribeConnectionAliases>
-- to make sure that the current state of the connection alias is
-- @CREATED@.
module Network.AWS.WorkSpaces.AssociateConnectionAlias
  ( -- * Creating a Request
    AssociateConnectionAlias (..),
    newAssociateConnectionAlias,

    -- * Request Lenses
    associateConnectionAlias_aliasId,
    associateConnectionAlias_resourceId,

    -- * Destructuring the Response
    AssociateConnectionAliasResponse (..),
    newAssociateConnectionAliasResponse,

    -- * Response Lenses
    associateConnectionAliasResponse_connectionIdentifier,
    associateConnectionAliasResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newAssociateConnectionAlias' smart constructor.
data AssociateConnectionAlias = AssociateConnectionAlias'
  { -- | The identifier of the connection alias.
    aliasId :: Prelude.Text,
    -- | The identifier of the directory to associate the connection alias with.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateConnectionAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasId', 'associateConnectionAlias_aliasId' - The identifier of the connection alias.
--
-- 'resourceId', 'associateConnectionAlias_resourceId' - The identifier of the directory to associate the connection alias with.
newAssociateConnectionAlias ::
  -- | 'aliasId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  AssociateConnectionAlias
newAssociateConnectionAlias pAliasId_ pResourceId_ =
  AssociateConnectionAlias'
    { aliasId = pAliasId_,
      resourceId = pResourceId_
    }

-- | The identifier of the connection alias.
associateConnectionAlias_aliasId :: Lens.Lens' AssociateConnectionAlias Prelude.Text
associateConnectionAlias_aliasId = Lens.lens (\AssociateConnectionAlias' {aliasId} -> aliasId) (\s@AssociateConnectionAlias' {} a -> s {aliasId = a} :: AssociateConnectionAlias)

-- | The identifier of the directory to associate the connection alias with.
associateConnectionAlias_resourceId :: Lens.Lens' AssociateConnectionAlias Prelude.Text
associateConnectionAlias_resourceId = Lens.lens (\AssociateConnectionAlias' {resourceId} -> resourceId) (\s@AssociateConnectionAlias' {} a -> s {resourceId = a} :: AssociateConnectionAlias)

instance Prelude.AWSRequest AssociateConnectionAlias where
  type
    Rs AssociateConnectionAlias =
      AssociateConnectionAliasResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateConnectionAliasResponse'
            Prelude.<$> (x Prelude..?> "ConnectionIdentifier")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateConnectionAlias

instance Prelude.NFData AssociateConnectionAlias

instance Prelude.ToHeaders AssociateConnectionAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkspacesService.AssociateConnectionAlias" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AssociateConnectionAlias where
  toJSON AssociateConnectionAlias' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AliasId" Prelude..= aliasId),
            Prelude.Just ("ResourceId" Prelude..= resourceId)
          ]
      )

instance Prelude.ToPath AssociateConnectionAlias where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AssociateConnectionAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateConnectionAliasResponse' smart constructor.
data AssociateConnectionAliasResponse = AssociateConnectionAliasResponse'
  { -- | The identifier of the connection alias association. You use the
    -- connection identifier in the DNS TXT record when you\'re configuring
    -- your DNS routing policies.
    connectionIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateConnectionAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionIdentifier', 'associateConnectionAliasResponse_connectionIdentifier' - The identifier of the connection alias association. You use the
-- connection identifier in the DNS TXT record when you\'re configuring
-- your DNS routing policies.
--
-- 'httpStatus', 'associateConnectionAliasResponse_httpStatus' - The response's http status code.
newAssociateConnectionAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateConnectionAliasResponse
newAssociateConnectionAliasResponse pHttpStatus_ =
  AssociateConnectionAliasResponse'
    { connectionIdentifier =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the connection alias association. You use the
-- connection identifier in the DNS TXT record when you\'re configuring
-- your DNS routing policies.
associateConnectionAliasResponse_connectionIdentifier :: Lens.Lens' AssociateConnectionAliasResponse (Prelude.Maybe Prelude.Text)
associateConnectionAliasResponse_connectionIdentifier = Lens.lens (\AssociateConnectionAliasResponse' {connectionIdentifier} -> connectionIdentifier) (\s@AssociateConnectionAliasResponse' {} a -> s {connectionIdentifier = a} :: AssociateConnectionAliasResponse)

-- | The response's http status code.
associateConnectionAliasResponse_httpStatus :: Lens.Lens' AssociateConnectionAliasResponse Prelude.Int
associateConnectionAliasResponse_httpStatus = Lens.lens (\AssociateConnectionAliasResponse' {httpStatus} -> httpStatus) (\s@AssociateConnectionAliasResponse' {} a -> s {httpStatus = a} :: AssociateConnectionAliasResponse)

instance
  Prelude.NFData
    AssociateConnectionAliasResponse

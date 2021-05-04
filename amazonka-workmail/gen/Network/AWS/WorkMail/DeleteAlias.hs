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
-- Module      : Network.AWS.WorkMail.DeleteAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove one or more specified aliases from a set of aliases for a given
-- user.
module Network.AWS.WorkMail.DeleteAlias
  ( -- * Creating a Request
    DeleteAlias (..),
    newDeleteAlias,

    -- * Request Lenses
    deleteAlias_organizationId,
    deleteAlias_entityId,
    deleteAlias_alias,

    -- * Destructuring the Response
    DeleteAliasResponse (..),
    newDeleteAliasResponse,

    -- * Response Lenses
    deleteAliasResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newDeleteAlias' smart constructor.
data DeleteAlias = DeleteAlias'
  { -- | The identifier for the organization under which the user exists.
    organizationId :: Prelude.Text,
    -- | The identifier for the member (user or group) from which to have the
    -- aliases removed.
    entityId :: Prelude.Text,
    -- | The aliases to be removed from the user\'s set of aliases. Duplicate
    -- entries in the list are collapsed into single entries (the list is
    -- transformed into a set).
    alias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'deleteAlias_organizationId' - The identifier for the organization under which the user exists.
--
-- 'entityId', 'deleteAlias_entityId' - The identifier for the member (user or group) from which to have the
-- aliases removed.
--
-- 'alias', 'deleteAlias_alias' - The aliases to be removed from the user\'s set of aliases. Duplicate
-- entries in the list are collapsed into single entries (the list is
-- transformed into a set).
newDeleteAlias ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'entityId'
  Prelude.Text ->
  -- | 'alias'
  Prelude.Text ->
  DeleteAlias
newDeleteAlias pOrganizationId_ pEntityId_ pAlias_ =
  DeleteAlias'
    { organizationId = pOrganizationId_,
      entityId = pEntityId_,
      alias = pAlias_
    }

-- | The identifier for the organization under which the user exists.
deleteAlias_organizationId :: Lens.Lens' DeleteAlias Prelude.Text
deleteAlias_organizationId = Lens.lens (\DeleteAlias' {organizationId} -> organizationId) (\s@DeleteAlias' {} a -> s {organizationId = a} :: DeleteAlias)

-- | The identifier for the member (user or group) from which to have the
-- aliases removed.
deleteAlias_entityId :: Lens.Lens' DeleteAlias Prelude.Text
deleteAlias_entityId = Lens.lens (\DeleteAlias' {entityId} -> entityId) (\s@DeleteAlias' {} a -> s {entityId = a} :: DeleteAlias)

-- | The aliases to be removed from the user\'s set of aliases. Duplicate
-- entries in the list are collapsed into single entries (the list is
-- transformed into a set).
deleteAlias_alias :: Lens.Lens' DeleteAlias Prelude.Text
deleteAlias_alias = Lens.lens (\DeleteAlias' {alias} -> alias) (\s@DeleteAlias' {} a -> s {alias = a} :: DeleteAlias)

instance Prelude.AWSRequest DeleteAlias where
  type Rs DeleteAlias = DeleteAliasResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAliasResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAlias

instance Prelude.NFData DeleteAlias

instance Prelude.ToHeaders DeleteAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkMailService.DeleteAlias" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteAlias where
  toJSON DeleteAlias' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Prelude..= organizationId),
            Prelude.Just ("EntityId" Prelude..= entityId),
            Prelude.Just ("Alias" Prelude..= alias)
          ]
      )

instance Prelude.ToPath DeleteAlias where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAliasResponse' smart constructor.
data DeleteAliasResponse = DeleteAliasResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAliasResponse_httpStatus' - The response's http status code.
newDeleteAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAliasResponse
newDeleteAliasResponse pHttpStatus_ =
  DeleteAliasResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteAliasResponse_httpStatus :: Lens.Lens' DeleteAliasResponse Prelude.Int
deleteAliasResponse_httpStatus = Lens.lens (\DeleteAliasResponse' {httpStatus} -> httpStatus) (\s@DeleteAliasResponse' {} a -> s {httpStatus = a} :: DeleteAliasResponse)

instance Prelude.NFData DeleteAliasResponse

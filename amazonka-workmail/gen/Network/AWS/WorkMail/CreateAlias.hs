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
-- Module      : Network.AWS.WorkMail.CreateAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an alias to the set of a given member (user or group) of Amazon
-- WorkMail.
module Network.AWS.WorkMail.CreateAlias
  ( -- * Creating a Request
    CreateAlias (..),
    newCreateAlias,

    -- * Request Lenses
    createAlias_organizationId,
    createAlias_entityId,
    createAlias_alias,

    -- * Destructuring the Response
    CreateAliasResponse (..),
    newCreateAliasResponse,

    -- * Response Lenses
    createAliasResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newCreateAlias' smart constructor.
data CreateAlias = CreateAlias'
  { -- | The organization under which the member (user or group) exists.
    organizationId :: Prelude.Text,
    -- | The member (user or group) to which this alias is added.
    entityId :: Prelude.Text,
    -- | The alias to add to the member set.
    alias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'createAlias_organizationId' - The organization under which the member (user or group) exists.
--
-- 'entityId', 'createAlias_entityId' - The member (user or group) to which this alias is added.
--
-- 'alias', 'createAlias_alias' - The alias to add to the member set.
newCreateAlias ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'entityId'
  Prelude.Text ->
  -- | 'alias'
  Prelude.Text ->
  CreateAlias
newCreateAlias pOrganizationId_ pEntityId_ pAlias_ =
  CreateAlias'
    { organizationId = pOrganizationId_,
      entityId = pEntityId_,
      alias = pAlias_
    }

-- | The organization under which the member (user or group) exists.
createAlias_organizationId :: Lens.Lens' CreateAlias Prelude.Text
createAlias_organizationId = Lens.lens (\CreateAlias' {organizationId} -> organizationId) (\s@CreateAlias' {} a -> s {organizationId = a} :: CreateAlias)

-- | The member (user or group) to which this alias is added.
createAlias_entityId :: Lens.Lens' CreateAlias Prelude.Text
createAlias_entityId = Lens.lens (\CreateAlias' {entityId} -> entityId) (\s@CreateAlias' {} a -> s {entityId = a} :: CreateAlias)

-- | The alias to add to the member set.
createAlias_alias :: Lens.Lens' CreateAlias Prelude.Text
createAlias_alias = Lens.lens (\CreateAlias' {alias} -> alias) (\s@CreateAlias' {} a -> s {alias = a} :: CreateAlias)

instance Prelude.AWSRequest CreateAlias where
  type Rs CreateAlias = CreateAliasResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateAliasResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAlias

instance Prelude.NFData CreateAlias

instance Prelude.ToHeaders CreateAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkMailService.CreateAlias" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateAlias where
  toJSON CreateAlias' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Prelude..= organizationId),
            Prelude.Just ("EntityId" Prelude..= entityId),
            Prelude.Just ("Alias" Prelude..= alias)
          ]
      )

instance Prelude.ToPath CreateAlias where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAliasResponse' smart constructor.
data CreateAliasResponse = CreateAliasResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAliasResponse_httpStatus' - The response's http status code.
newCreateAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAliasResponse
newCreateAliasResponse pHttpStatus_ =
  CreateAliasResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createAliasResponse_httpStatus :: Lens.Lens' CreateAliasResponse Prelude.Int
createAliasResponse_httpStatus = Lens.lens (\CreateAliasResponse' {httpStatus} -> httpStatus) (\s@CreateAliasResponse' {} a -> s {httpStatus = a} :: CreateAliasResponse)

instance Prelude.NFData CreateAliasResponse

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
-- Module      : Network.AWS.KMS.DeleteAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified alias.
--
-- Because an alias is not a property of a CMK, you can delete and change
-- the aliases of a CMK without affecting the CMK. Also, aliases do not
-- appear in the response from the DescribeKey operation. To get the
-- aliases of all CMKs, use the ListAliases operation.
--
-- Each CMK can have multiple aliases. To change the alias of a CMK, use
-- DeleteAlias to delete the current alias and CreateAlias to create a new
-- alias. To associate an existing alias with a different customer master
-- key (CMK), call UpdateAlias.
--
-- __Cross-account use__: No. You cannot perform this operation on an alias
-- in a different AWS account.
--
-- __Required permissions__
--
-- -   <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:DeleteAlias>
--     on the alias (IAM policy).
--
-- -   <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:DeleteAlias>
--     on the CMK (key policy).
--
-- For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-alias.html#alias-access Controlling access to aliases>
-- in the /AWS Key Management Service Developer Guide/.
--
-- __Related operations:__
--
-- -   CreateAlias
--
-- -   ListAliases
--
-- -   UpdateAlias
module Network.AWS.KMS.DeleteAlias
  ( -- * Creating a Request
    DeleteAlias (..),
    newDeleteAlias,

    -- * Request Lenses
    deleteAlias_aliasName,

    -- * Destructuring the Response
    DeleteAliasResponse (..),
    newDeleteAliasResponse,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAlias' smart constructor.
data DeleteAlias = DeleteAlias'
  { -- | The alias to be deleted. The alias name must begin with @alias\/@
    -- followed by the alias name, such as @alias\/ExampleAlias@.
    aliasName :: Prelude.Text
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
-- 'aliasName', 'deleteAlias_aliasName' - The alias to be deleted. The alias name must begin with @alias\/@
-- followed by the alias name, such as @alias\/ExampleAlias@.
newDeleteAlias ::
  -- | 'aliasName'
  Prelude.Text ->
  DeleteAlias
newDeleteAlias pAliasName_ =
  DeleteAlias' {aliasName = pAliasName_}

-- | The alias to be deleted. The alias name must begin with @alias\/@
-- followed by the alias name, such as @alias\/ExampleAlias@.
deleteAlias_aliasName :: Lens.Lens' DeleteAlias Prelude.Text
deleteAlias_aliasName = Lens.lens (\DeleteAlias' {aliasName} -> aliasName) (\s@DeleteAlias' {} a -> s {aliasName = a} :: DeleteAlias)

instance Prelude.AWSRequest DeleteAlias where
  type Rs DeleteAlias = DeleteAliasResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull DeleteAliasResponse'

instance Prelude.Hashable DeleteAlias

instance Prelude.NFData DeleteAlias

instance Prelude.ToHeaders DeleteAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("TrentService.DeleteAlias" :: Prelude.ByteString),
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
          [Prelude.Just ("AliasName" Prelude..= aliasName)]
      )

instance Prelude.ToPath DeleteAlias where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAliasResponse' smart constructor.
data DeleteAliasResponse = DeleteAliasResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAliasResponse ::
  DeleteAliasResponse
newDeleteAliasResponse = DeleteAliasResponse'

instance Prelude.NFData DeleteAliasResponse

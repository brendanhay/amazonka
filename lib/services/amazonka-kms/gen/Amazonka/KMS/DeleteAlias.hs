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
-- Module      : Amazonka.KMS.DeleteAlias
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified alias.
--
-- Adding, deleting, or updating an alias can allow or deny permission to
-- the KMS key. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/abac.html ABAC in KMS>
-- in the /Key Management Service Developer Guide/.
--
-- Because an alias is not a property of a KMS key, you can delete and
-- change the aliases of a KMS key without affecting the KMS key. Also,
-- aliases do not appear in the response from the DescribeKey operation. To
-- get the aliases of all KMS keys, use the ListAliases operation.
--
-- Each KMS key can have multiple aliases. To change the alias of a KMS
-- key, use DeleteAlias to delete the current alias and CreateAlias to
-- create a new alias. To associate an existing alias with a different KMS
-- key, call UpdateAlias.
--
-- __Cross-account use__: No. You cannot perform this operation on an alias
-- in a different Amazon Web Services account.
--
-- __Required permissions__
--
-- -   <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:DeleteAlias>
--     on the alias (IAM policy).
--
-- -   <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:DeleteAlias>
--     on the KMS key (key policy).
--
-- For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-alias.html#alias-access Controlling access to aliases>
-- in the /Key Management Service Developer Guide/.
--
-- __Related operations:__
--
-- -   CreateAlias
--
-- -   ListAliases
--
-- -   UpdateAlias
module Amazonka.KMS.DeleteAlias
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAlias' smart constructor.
data DeleteAlias = DeleteAlias'
  { -- | The alias to be deleted. The alias name must begin with @alias\/@
    -- followed by the alias name, such as @alias\/ExampleAlias@.
    aliasName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteAlias where
  type AWSResponse DeleteAlias = DeleteAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeleteAliasResponse'

instance Prelude.Hashable DeleteAlias where
  hashWithSalt _salt DeleteAlias' {..} =
    _salt `Prelude.hashWithSalt` aliasName

instance Prelude.NFData DeleteAlias where
  rnf DeleteAlias' {..} = Prelude.rnf aliasName

instance Data.ToHeaders DeleteAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("TrentService.DeleteAlias" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAlias where
  toJSON DeleteAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("AliasName" Data..= aliasName)]
      )

instance Data.ToPath DeleteAlias where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAliasResponse' smart constructor.
data DeleteAliasResponse = DeleteAliasResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAliasResponse ::
  DeleteAliasResponse
newDeleteAliasResponse = DeleteAliasResponse'

instance Prelude.NFData DeleteAliasResponse where
  rnf _ = ()

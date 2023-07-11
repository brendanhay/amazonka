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
-- Module      : Amazonka.IAM.UntagRole
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the role. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
module Amazonka.IAM.UntagRole
  ( -- * Creating a Request
    UntagRole (..),
    newUntagRole,

    -- * Request Lenses
    untagRole_roleName,
    untagRole_tagKeys,

    -- * Destructuring the Response
    UntagRoleResponse (..),
    newUntagRoleResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUntagRole' smart constructor.
data UntagRole = UntagRole'
  { -- | The name of the IAM role from which you want to remove tags.
    --
    -- This parameter accepts (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that consist of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    roleName :: Prelude.Text,
    -- | A list of key names as a simple array of strings. The tags with matching
    -- keys are removed from the specified role.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleName', 'untagRole_roleName' - The name of the IAM role from which you want to remove tags.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'tagKeys', 'untagRole_tagKeys' - A list of key names as a simple array of strings. The tags with matching
-- keys are removed from the specified role.
newUntagRole ::
  -- | 'roleName'
  Prelude.Text ->
  UntagRole
newUntagRole pRoleName_ =
  UntagRole'
    { roleName = pRoleName_,
      tagKeys = Prelude.mempty
    }

-- | The name of the IAM role from which you want to remove tags.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
untagRole_roleName :: Lens.Lens' UntagRole Prelude.Text
untagRole_roleName = Lens.lens (\UntagRole' {roleName} -> roleName) (\s@UntagRole' {} a -> s {roleName = a} :: UntagRole)

-- | A list of key names as a simple array of strings. The tags with matching
-- keys are removed from the specified role.
untagRole_tagKeys :: Lens.Lens' UntagRole [Prelude.Text]
untagRole_tagKeys = Lens.lens (\UntagRole' {tagKeys} -> tagKeys) (\s@UntagRole' {} a -> s {tagKeys = a} :: UntagRole) Prelude.. Lens.coerced

instance Core.AWSRequest UntagRole where
  type AWSResponse UntagRole = UntagRoleResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response = Response.receiveNull UntagRoleResponse'

instance Prelude.Hashable UntagRole where
  hashWithSalt _salt UntagRole' {..} =
    _salt
      `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData UntagRole where
  rnf UntagRole' {..} =
    Prelude.rnf roleName
      `Prelude.seq` Prelude.rnf tagKeys

instance Data.ToHeaders UntagRole where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UntagRole where
  toPath = Prelude.const "/"

instance Data.ToQuery UntagRole where
  toQuery UntagRole' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("UntagRole" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "RoleName" Data.=: roleName,
        "TagKeys" Data.=: Data.toQueryList "member" tagKeys
      ]

-- | /See:/ 'newUntagRoleResponse' smart constructor.
data UntagRoleResponse = UntagRoleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUntagRoleResponse ::
  UntagRoleResponse
newUntagRoleResponse = UntagRoleResponse'

instance Prelude.NFData UntagRoleResponse where
  rnf _ = ()

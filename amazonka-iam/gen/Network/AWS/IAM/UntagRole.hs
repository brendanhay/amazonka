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
-- Module      : Network.AWS.IAM.UntagRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the role. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
module Network.AWS.IAM.UntagRole
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

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
untagRole_tagKeys = Lens.lens (\UntagRole' {tagKeys} -> tagKeys) (\s@UntagRole' {} a -> s {tagKeys = a} :: UntagRole) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest UntagRole where
  type Rs UntagRole = UntagRoleResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull UntagRoleResponse'

instance Prelude.Hashable UntagRole

instance Prelude.NFData UntagRole

instance Prelude.ToHeaders UntagRole where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath UntagRole where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UntagRole where
  toQuery UntagRole' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("UntagRole" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "RoleName" Prelude.=: roleName,
        "TagKeys"
          Prelude.=: Prelude.toQueryList "member" tagKeys
      ]

-- | /See:/ 'newUntagRoleResponse' smart constructor.
data UntagRoleResponse = UntagRoleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UntagRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUntagRoleResponse ::
  UntagRoleResponse
newUntagRoleResponse = UntagRoleResponse'

instance Prelude.NFData UntagRoleResponse

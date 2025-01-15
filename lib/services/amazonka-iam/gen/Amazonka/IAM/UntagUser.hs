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
-- Module      : Amazonka.IAM.UntagUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the user. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
module Amazonka.IAM.UntagUser
  ( -- * Creating a Request
    UntagUser (..),
    newUntagUser,

    -- * Request Lenses
    untagUser_userName,
    untagUser_tagKeys,

    -- * Destructuring the Response
    UntagUserResponse (..),
    newUntagUserResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUntagUser' smart constructor.
data UntagUser = UntagUser'
  { -- | The name of the IAM user from which you want to remove tags.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Text,
    -- | A list of key names as a simple array of strings. The tags with matching
    -- keys are removed from the specified user.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'untagUser_userName' - The name of the IAM user from which you want to remove tags.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'tagKeys', 'untagUser_tagKeys' - A list of key names as a simple array of strings. The tags with matching
-- keys are removed from the specified user.
newUntagUser ::
  -- | 'userName'
  Prelude.Text ->
  UntagUser
newUntagUser pUserName_ =
  UntagUser'
    { userName = pUserName_,
      tagKeys = Prelude.mempty
    }

-- | The name of the IAM user from which you want to remove tags.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
untagUser_userName :: Lens.Lens' UntagUser Prelude.Text
untagUser_userName = Lens.lens (\UntagUser' {userName} -> userName) (\s@UntagUser' {} a -> s {userName = a} :: UntagUser)

-- | A list of key names as a simple array of strings. The tags with matching
-- keys are removed from the specified user.
untagUser_tagKeys :: Lens.Lens' UntagUser [Prelude.Text]
untagUser_tagKeys = Lens.lens (\UntagUser' {tagKeys} -> tagKeys) (\s@UntagUser' {} a -> s {tagKeys = a} :: UntagUser) Prelude.. Lens.coerced

instance Core.AWSRequest UntagUser where
  type AWSResponse UntagUser = UntagUserResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response = Response.receiveNull UntagUserResponse'

instance Prelude.Hashable UntagUser where
  hashWithSalt _salt UntagUser' {..} =
    _salt
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData UntagUser where
  rnf UntagUser' {..} =
    Prelude.rnf userName `Prelude.seq`
      Prelude.rnf tagKeys

instance Data.ToHeaders UntagUser where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UntagUser where
  toPath = Prelude.const "/"

instance Data.ToQuery UntagUser where
  toQuery UntagUser' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("UntagUser" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Data.=: userName,
        "TagKeys" Data.=: Data.toQueryList "member" tagKeys
      ]

-- | /See:/ 'newUntagUserResponse' smart constructor.
data UntagUserResponse = UntagUserResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUntagUserResponse ::
  UntagUserResponse
newUntagUserResponse = UntagUserResponse'

instance Prelude.NFData UntagUserResponse where
  rnf _ = ()

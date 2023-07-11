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
-- Module      : Amazonka.IAM.UntagInstanceProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the IAM instance profile. For more
-- information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
module Amazonka.IAM.UntagInstanceProfile
  ( -- * Creating a Request
    UntagInstanceProfile (..),
    newUntagInstanceProfile,

    -- * Request Lenses
    untagInstanceProfile_instanceProfileName,
    untagInstanceProfile_tagKeys,

    -- * Destructuring the Response
    UntagInstanceProfileResponse (..),
    newUntagInstanceProfileResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUntagInstanceProfile' smart constructor.
data UntagInstanceProfile = UntagInstanceProfile'
  { -- | The name of the IAM instance profile from which you want to remove tags.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    instanceProfileName :: Prelude.Text,
    -- | A list of key names as a simple array of strings. The tags with matching
    -- keys are removed from the specified instance profile.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagInstanceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceProfileName', 'untagInstanceProfile_instanceProfileName' - The name of the IAM instance profile from which you want to remove tags.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'tagKeys', 'untagInstanceProfile_tagKeys' - A list of key names as a simple array of strings. The tags with matching
-- keys are removed from the specified instance profile.
newUntagInstanceProfile ::
  -- | 'instanceProfileName'
  Prelude.Text ->
  UntagInstanceProfile
newUntagInstanceProfile pInstanceProfileName_ =
  UntagInstanceProfile'
    { instanceProfileName =
        pInstanceProfileName_,
      tagKeys = Prelude.mempty
    }

-- | The name of the IAM instance profile from which you want to remove tags.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
untagInstanceProfile_instanceProfileName :: Lens.Lens' UntagInstanceProfile Prelude.Text
untagInstanceProfile_instanceProfileName = Lens.lens (\UntagInstanceProfile' {instanceProfileName} -> instanceProfileName) (\s@UntagInstanceProfile' {} a -> s {instanceProfileName = a} :: UntagInstanceProfile)

-- | A list of key names as a simple array of strings. The tags with matching
-- keys are removed from the specified instance profile.
untagInstanceProfile_tagKeys :: Lens.Lens' UntagInstanceProfile [Prelude.Text]
untagInstanceProfile_tagKeys = Lens.lens (\UntagInstanceProfile' {tagKeys} -> tagKeys) (\s@UntagInstanceProfile' {} a -> s {tagKeys = a} :: UntagInstanceProfile) Prelude.. Lens.coerced

instance Core.AWSRequest UntagInstanceProfile where
  type
    AWSResponse UntagInstanceProfile =
      UntagInstanceProfileResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull UntagInstanceProfileResponse'

instance Prelude.Hashable UntagInstanceProfile where
  hashWithSalt _salt UntagInstanceProfile' {..} =
    _salt
      `Prelude.hashWithSalt` instanceProfileName
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData UntagInstanceProfile where
  rnf UntagInstanceProfile' {..} =
    Prelude.rnf instanceProfileName
      `Prelude.seq` Prelude.rnf tagKeys

instance Data.ToHeaders UntagInstanceProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UntagInstanceProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery UntagInstanceProfile where
  toQuery UntagInstanceProfile' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("UntagInstanceProfile" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "InstanceProfileName" Data.=: instanceProfileName,
        "TagKeys" Data.=: Data.toQueryList "member" tagKeys
      ]

-- | /See:/ 'newUntagInstanceProfileResponse' smart constructor.
data UntagInstanceProfileResponse = UntagInstanceProfileResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagInstanceProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUntagInstanceProfileResponse ::
  UntagInstanceProfileResponse
newUntagInstanceProfileResponse =
  UntagInstanceProfileResponse'

instance Prelude.NFData UntagInstanceProfileResponse where
  rnf _ = ()

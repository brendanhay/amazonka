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
-- Module      : Network.AWS.IAM.UntagInstanceProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the IAM instance profile. For more
-- information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
module Network.AWS.IAM.UntagInstanceProfile
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

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUntagInstanceProfile' smart constructor.
data UntagInstanceProfile = UntagInstanceProfile'
  { -- | The name of the IAM instance profile from which you want to remove tags.
    --
    -- This parameter accepts (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that consist of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: =,.\@-
    instanceProfileName :: Prelude.Text,
    -- | A list of key names as a simple array of strings. The tags with matching
    -- keys are removed from the specified instance profile.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
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
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
untagInstanceProfile_instanceProfileName :: Lens.Lens' UntagInstanceProfile Prelude.Text
untagInstanceProfile_instanceProfileName = Lens.lens (\UntagInstanceProfile' {instanceProfileName} -> instanceProfileName) (\s@UntagInstanceProfile' {} a -> s {instanceProfileName = a} :: UntagInstanceProfile)

-- | A list of key names as a simple array of strings. The tags with matching
-- keys are removed from the specified instance profile.
untagInstanceProfile_tagKeys :: Lens.Lens' UntagInstanceProfile [Prelude.Text]
untagInstanceProfile_tagKeys = Lens.lens (\UntagInstanceProfile' {tagKeys} -> tagKeys) (\s@UntagInstanceProfile' {} a -> s {tagKeys = a} :: UntagInstanceProfile) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest UntagInstanceProfile where
  type
    Rs UntagInstanceProfile =
      UntagInstanceProfileResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull UntagInstanceProfileResponse'

instance Prelude.Hashable UntagInstanceProfile

instance Prelude.NFData UntagInstanceProfile

instance Prelude.ToHeaders UntagInstanceProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath UntagInstanceProfile where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UntagInstanceProfile where
  toQuery UntagInstanceProfile' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("UntagInstanceProfile" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "InstanceProfileName" Prelude.=: instanceProfileName,
        "TagKeys"
          Prelude.=: Prelude.toQueryList "member" tagKeys
      ]

-- | /See:/ 'newUntagInstanceProfileResponse' smart constructor.
data UntagInstanceProfileResponse = UntagInstanceProfileResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UntagInstanceProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUntagInstanceProfileResponse ::
  UntagInstanceProfileResponse
newUntagInstanceProfileResponse =
  UntagInstanceProfileResponse'

instance Prelude.NFData UntagInstanceProfileResponse

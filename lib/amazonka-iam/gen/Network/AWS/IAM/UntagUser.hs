{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UntagUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the user. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
module Network.AWS.IAM.UntagUser
  ( -- * Creating a request
    UntagUser (..),
    mkUntagUser,

    -- ** Request lenses
    uufTagKeys,
    uufUserName,

    -- * Destructuring the response
    UntagUserResponse (..),
    mkUntagUserResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUntagUser' smart constructor.
data UntagUser = UntagUser'
  { -- | A list of key names as a simple array of strings. The tags with matching keys are removed from the specified user.
    tagKeys :: [Lude.Text],
    -- | The name of the IAM user from which you want to remove tags.
    --
    -- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@-
    userName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagUser' with the minimum fields required to make a request.
--
-- * 'tagKeys' - A list of key names as a simple array of strings. The tags with matching keys are removed from the specified user.
-- * 'userName' - The name of the IAM user from which you want to remove tags.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@-
mkUntagUser ::
  -- | 'userName'
  Lude.Text ->
  UntagUser
mkUntagUser pUserName_ =
  UntagUser' {tagKeys = Lude.mempty, userName = pUserName_}

-- | A list of key names as a simple array of strings. The tags with matching keys are removed from the specified user.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uufTagKeys :: Lens.Lens' UntagUser [Lude.Text]
uufTagKeys = Lens.lens (tagKeys :: UntagUser -> [Lude.Text]) (\s a -> s {tagKeys = a} :: UntagUser)
{-# DEPRECATED uufTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | The name of the IAM user from which you want to remove tags.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uufUserName :: Lens.Lens' UntagUser Lude.Text
uufUserName = Lens.lens (userName :: UntagUser -> Lude.Text) (\s a -> s {userName = a} :: UntagUser)
{-# DEPRECATED uufUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Lude.AWSRequest UntagUser where
  type Rs UntagUser = UntagUserResponse
  request = Req.postQuery iamService
  response = Res.receiveNull UntagUserResponse'

instance Lude.ToHeaders UntagUser where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UntagUser where
  toPath = Lude.const "/"

instance Lude.ToQuery UntagUser where
  toQuery UntagUser' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UntagUser" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "TagKeys" Lude.=: Lude.toQueryList "member" tagKeys,
        "UserName" Lude.=: userName
      ]

-- | /See:/ 'mkUntagUserResponse' smart constructor.
data UntagUserResponse = UntagUserResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagUserResponse' with the minimum fields required to make a request.
mkUntagUserResponse ::
  UntagUserResponse
mkUntagUserResponse = UntagUserResponse'

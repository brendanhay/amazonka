{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.TagUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to an IAM user. If a tag with the same key name already exists, then that tag is overwritten with the new value.
--
-- A tag consists of a key name and an associated value. By assigning tags to your resources, you can do the following:
--
--     * __Administrative grouping and discovery__ - Attach tags to resources to aid in organization and search. For example, you could search for all resources with the key name /Project/ and the value /MyImportantProject/ . Or search for all resources with the key name /Cost Center/ and the value /41200/ .
--
--
--     * __Access control__ - Reference tags in IAM user-based and resource-based policies. You can use tags to restrict access to only an IAM requesting user or to a role that has a specified tag attached. You can also restrict access to only those resources that have a certain tag attached. For examples of policies that show how to use tags to control access, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Control Access Using IAM Tags> in the /IAM User Guide/ .
--
--
--     * __Cost allocation__ - Use tags to help track which individuals and teams are using which AWS resources.
--
--
-- For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
module Network.AWS.IAM.TagUser
  ( -- * Creating a request
    TagUser (..),
    mkTagUser,

    -- ** Request lenses
    tuUserName,
    tuTags,

    -- * Destructuring the response
    TagUserResponse (..),
    mkTagUserResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTagUser' smart constructor.
data TagUser = TagUser' {userName :: Lude.Text, tags :: [Tag]}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagUser' with the minimum fields required to make a request.
--
-- * 'tags' - The list of tags that you want to attach to the user. Each tag consists of a key name and an associated value.
-- * 'userName' - The name of the user that you want to add tags to.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@-
mkTagUser ::
  -- | 'userName'
  Lude.Text ->
  TagUser
mkTagUser pUserName_ =
  TagUser' {userName = pUserName_, tags = Lude.mempty}

-- | The name of the user that you want to add tags to.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tuUserName :: Lens.Lens' TagUser Lude.Text
tuUserName = Lens.lens (userName :: TagUser -> Lude.Text) (\s a -> s {userName = a} :: TagUser)
{-# DEPRECATED tuUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The list of tags that you want to attach to the user. Each tag consists of a key name and an associated value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tuTags :: Lens.Lens' TagUser [Tag]
tuTags = Lens.lens (tags :: TagUser -> [Tag]) (\s a -> s {tags = a} :: TagUser)
{-# DEPRECATED tuTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest TagUser where
  type Rs TagUser = TagUserResponse
  request = Req.postQuery iamService
  response = Res.receiveNull TagUserResponse'

instance Lude.ToHeaders TagUser where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath TagUser where
  toPath = Lude.const "/"

instance Lude.ToQuery TagUser where
  toQuery TagUser' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("TagUser" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName,
        "Tags" Lude.=: Lude.toQueryList "member" tags
      ]

-- | /See:/ 'mkTagUserResponse' smart constructor.
data TagUserResponse = TagUserResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagUserResponse' with the minimum fields required to make a request.
mkTagUserResponse ::
  TagUserResponse
mkTagUserResponse = TagUserResponse'

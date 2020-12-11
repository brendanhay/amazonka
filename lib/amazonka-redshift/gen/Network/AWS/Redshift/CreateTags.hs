{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to a cluster.
--
-- A resource can have up to 50 tags. If you try to create more than 50 tags for a resource, you will receive an error and the attempt will fail.
-- If you specify a key that already exists for the resource, the value for that key will be updated with the new value.
module Network.AWS.Redshift.CreateTags
  ( -- * Creating a request
    CreateTags (..),
    mkCreateTags,

    -- ** Request lenses
    ctResourceName,
    ctTags,

    -- * Destructuring the response
    CreateTagsResponse (..),
    mkCreateTagsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the output from the @CreateTags@ action.
--
-- /See:/ 'mkCreateTags' smart constructor.
data CreateTags = CreateTags'
  { resourceName :: Lude.Text,
    tags :: [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTags' with the minimum fields required to make a request.
--
-- * 'resourceName' - The Amazon Resource Name (ARN) to which you want to add the tag or tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@ .
-- * 'tags' - One or more name/value pairs to add as tags to the specified resource. Each tag name is passed in with the parameter @Key@ and the corresponding value is passed in with the parameter @Value@ . The @Key@ and @Value@ parameters are separated by a comma (,). Separate multiple tags with a space. For example, @--tags "Key"="owner","Value"="admin" "Key"="environment","Value"="test" "Key"="version","Value"="1.0"@ .
mkCreateTags ::
  -- | 'resourceName'
  Lude.Text ->
  CreateTags
mkCreateTags pResourceName_ =
  CreateTags' {resourceName = pResourceName_, tags = Lude.mempty}

-- | The Amazon Resource Name (ARN) to which you want to add the tag or tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@ .
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctResourceName :: Lens.Lens' CreateTags Lude.Text
ctResourceName = Lens.lens (resourceName :: CreateTags -> Lude.Text) (\s a -> s {resourceName = a} :: CreateTags)
{-# DEPRECATED ctResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | One or more name/value pairs to add as tags to the specified resource. Each tag name is passed in with the parameter @Key@ and the corresponding value is passed in with the parameter @Value@ . The @Key@ and @Value@ parameters are separated by a comma (,). Separate multiple tags with a space. For example, @--tags "Key"="owner","Value"="admin" "Key"="environment","Value"="test" "Key"="version","Value"="1.0"@ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTags :: Lens.Lens' CreateTags [Tag]
ctTags = Lens.lens (tags :: CreateTags -> [Tag]) (\s a -> s {tags = a} :: CreateTags)
{-# DEPRECATED ctTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateTags where
  type Rs CreateTags = CreateTagsResponse
  request = Req.postQuery redshiftService
  response = Res.receiveNull CreateTagsResponse'

instance Lude.ToHeaders CreateTags where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateTags where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTags where
  toQuery CreateTags' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateTags" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ResourceName" Lude.=: resourceName,
        "Tags" Lude.=: Lude.toQueryList "Tag" tags
      ]

-- | /See:/ 'mkCreateTagsResponse' smart constructor.
data CreateTagsResponse = CreateTagsResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTagsResponse' with the minimum fields required to make a request.
mkCreateTagsResponse ::
  CreateTagsResponse
mkCreateTagsResponse = CreateTagsResponse'

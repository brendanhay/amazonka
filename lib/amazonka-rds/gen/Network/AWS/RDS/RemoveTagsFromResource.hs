{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RemoveTagsFromResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes metadata tags from an Amazon RDS resource.
--
-- For an overview on tagging an Amazon RDS resource, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.Tagging.html Tagging Amazon RDS Resources> in the /Amazon RDS User Guide./
module Network.AWS.RDS.RemoveTagsFromResource
  ( -- * Creating a request
    RemoveTagsFromResource (..),
    mkRemoveTagsFromResource,

    -- ** Request lenses
    rtfrResourceName,
    rtfrTagKeys,

    -- * Destructuring the response
    RemoveTagsFromResourceResponse (..),
    mkRemoveTagsFromResourceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { resourceName ::
      Lude.Text,
    tagKeys :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTagsFromResource' with the minimum fields required to make a request.
--
-- * 'resourceName' - The Amazon RDS resource that the tags are removed from. This value is an Amazon Resource Name (ARN). For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS> in the /Amazon RDS User Guide./
-- * 'tagKeys' - The tag key (name) of the tag to be removed.
mkRemoveTagsFromResource ::
  -- | 'resourceName'
  Lude.Text ->
  RemoveTagsFromResource
mkRemoveTagsFromResource pResourceName_ =
  RemoveTagsFromResource'
    { resourceName = pResourceName_,
      tagKeys = Lude.mempty
    }

-- | The Amazon RDS resource that the tags are removed from. This value is an Amazon Resource Name (ARN). For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrResourceName :: Lens.Lens' RemoveTagsFromResource Lude.Text
rtfrResourceName = Lens.lens (resourceName :: RemoveTagsFromResource -> Lude.Text) (\s a -> s {resourceName = a} :: RemoveTagsFromResource)
{-# DEPRECATED rtfrResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | The tag key (name) of the tag to be removed.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrTagKeys :: Lens.Lens' RemoveTagsFromResource [Lude.Text]
rtfrTagKeys = Lens.lens (tagKeys :: RemoveTagsFromResource -> [Lude.Text]) (\s a -> s {tagKeys = a} :: RemoveTagsFromResource)
{-# DEPRECATED rtfrTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Lude.AWSRequest RemoveTagsFromResource where
  type Rs RemoveTagsFromResource = RemoveTagsFromResourceResponse
  request = Req.postQuery rdsService
  response = Res.receiveNull RemoveTagsFromResourceResponse'

instance Lude.ToHeaders RemoveTagsFromResource where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RemoveTagsFromResource where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveTagsFromResource where
  toQuery RemoveTagsFromResource' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RemoveTagsFromResource" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "ResourceName" Lude.=: resourceName,
        "TagKeys" Lude.=: Lude.toQueryList "member" tagKeys
      ]

-- | /See:/ 'mkRemoveTagsFromResourceResponse' smart constructor.
data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTagsFromResourceResponse' with the minimum fields required to make a request.
mkRemoveTagsFromResourceResponse ::
  RemoveTagsFromResourceResponse
mkRemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'

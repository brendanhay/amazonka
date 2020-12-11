{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.UntagResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the specified resources. When you specify a tag key, the action removes both that key and its associated value. The operation succeeds even if you attempt to remove tags from a resource that were already removed. Note the following:
--
--
--     * To remove tags from a resource, you need the necessary permissions for the service that the resource belongs to as well as permissions for removing tags. For more information, see <http://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/Welcome.html this list> .
--
--
--     * You can only tag resources that are located in the specified Region for the AWS account.
module Network.AWS.ResourceGroupsTagging.UntagResources
  ( -- * Creating a request
    UntagResources (..),
    mkUntagResources,

    -- ** Request lenses
    urResourceARNList,
    urTagKeys,

    -- * Destructuring the response
    UntagResourcesResponse (..),
    mkUntagResourcesResponse,

    -- ** Response lenses
    urrsFailedResourcesMap,
    urrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import Network.AWS.ResourceGroupsTagging.Types
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUntagResources' smart constructor.
data UntagResources = UntagResources'
  { resourceARNList ::
      Lude.NonEmpty Lude.Text,
    tagKeys :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagResources' with the minimum fields required to make a request.
--
-- * 'resourceARNList' - A list of ARNs. An ARN (Amazon Resource Name) uniquely identifies a resource. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
-- * 'tagKeys' - A list of the tag keys that you want to remove from the specified resources.
mkUntagResources ::
  -- | 'resourceARNList'
  Lude.NonEmpty Lude.Text ->
  -- | 'tagKeys'
  Lude.NonEmpty Lude.Text ->
  UntagResources
mkUntagResources pResourceARNList_ pTagKeys_ =
  UntagResources'
    { resourceARNList = pResourceARNList_,
      tagKeys = pTagKeys_
    }

-- | A list of ARNs. An ARN (Amazon Resource Name) uniquely identifies a resource. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'resourceARNList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResourceARNList :: Lens.Lens' UntagResources (Lude.NonEmpty Lude.Text)
urResourceARNList = Lens.lens (resourceARNList :: UntagResources -> Lude.NonEmpty Lude.Text) (\s a -> s {resourceARNList = a} :: UntagResources)
{-# DEPRECATED urResourceARNList "Use generic-lens or generic-optics with 'resourceARNList' instead." #-}

-- | A list of the tag keys that you want to remove from the specified resources.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urTagKeys :: Lens.Lens' UntagResources (Lude.NonEmpty Lude.Text)
urTagKeys = Lens.lens (tagKeys :: UntagResources -> Lude.NonEmpty Lude.Text) (\s a -> s {tagKeys = a} :: UntagResources)
{-# DEPRECATED urTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Lude.AWSRequest UntagResources where
  type Rs UntagResources = UntagResourcesResponse
  request = Req.postJSON resourceGroupsTaggingService
  response =
    Res.receiveJSON
      ( \s h x ->
          UntagResourcesResponse'
            Lude.<$> (x Lude..?> "FailedResourcesMap" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UntagResources where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "ResourceGroupsTaggingAPI_20170126.UntagResources" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UntagResources where
  toJSON UntagResources' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceARNList" Lude..= resourceARNList),
            Lude.Just ("TagKeys" Lude..= tagKeys)
          ]
      )

instance Lude.ToPath UntagResources where
  toPath = Lude.const "/"

instance Lude.ToQuery UntagResources where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUntagResourcesResponse' smart constructor.
data UntagResourcesResponse = UntagResourcesResponse'
  { failedResourcesMap ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (FailureInfo)),
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagResourcesResponse' with the minimum fields required to make a request.
--
-- * 'failedResourcesMap' - Details of resources that could not be untagged. An error code, status code, and error message are returned for each failed item.
-- * 'responseStatus' - The response status code.
mkUntagResourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UntagResourcesResponse
mkUntagResourcesResponse pResponseStatus_ =
  UntagResourcesResponse'
    { failedResourcesMap = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Details of resources that could not be untagged. An error code, status code, and error message are returned for each failed item.
--
-- /Note:/ Consider using 'failedResourcesMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsFailedResourcesMap :: Lens.Lens' UntagResourcesResponse (Lude.Maybe (Lude.HashMap Lude.Text (FailureInfo)))
urrsFailedResourcesMap = Lens.lens (failedResourcesMap :: UntagResourcesResponse -> Lude.Maybe (Lude.HashMap Lude.Text (FailureInfo))) (\s a -> s {failedResourcesMap = a} :: UntagResourcesResponse)
{-# DEPRECATED urrsFailedResourcesMap "Use generic-lens or generic-optics with 'failedResourcesMap' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsResponseStatus :: Lens.Lens' UntagResourcesResponse Lude.Int
urrsResponseStatus = Lens.lens (responseStatus :: UntagResourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UntagResourcesResponse)
{-# DEPRECATED urrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.TagResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies one or more tags to the specified resources. Note the following:
--
--
--     * Not all resources can have tags. For a list of services that support tagging, see <http://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/Welcome.html this list> .
--
--
--     * Each resource can have up to 50 tags. For other limits, see <http://docs.aws.amazon.com/general/latest/gr/aws_tagging.html#tag-conventions Tag Naming and Usage Conventions> in the /AWS General Reference./
--
--
--     * You can only tag resources that are located in the specified Region for the AWS account.
--
--
--     * To add tags to a resource, you need the necessary permissions for the service that the resource belongs to as well as permissions for adding tags. For more information, see <http://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/Welcome.html this list> .
--
--
-- /Important:/ Do not store personally identifiable information (PII) or other confidential or sensitive information in tags. We use tags to provide you with billing and administration services. Tags are not intended to be used for private or sensitive data.
module Network.AWS.ResourceGroupsTagging.TagResources
  ( -- * Creating a request
    TagResources (..),
    mkTagResources,

    -- ** Request lenses
    trResourceARNList,
    trTags,

    -- * Destructuring the response
    TagResourcesResponse (..),
    mkTagResourcesResponse,

    -- ** Response lenses
    trrsFailedResourcesMap,
    trrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import Network.AWS.ResourceGroupsTagging.Types
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTagResources' smart constructor.
data TagResources = TagResources'
  { -- | A list of ARNs. An ARN (Amazon Resource Name) uniquely identifies a resource. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    resourceARNList :: Lude.NonEmpty Lude.Text,
    -- | The tags that you want to add to the specified resources. A tag consists of a key and a value that you define.
    tags :: Lude.HashMap Lude.Text (Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagResources' with the minimum fields required to make a request.
--
-- * 'resourceARNList' - A list of ARNs. An ARN (Amazon Resource Name) uniquely identifies a resource. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
-- * 'tags' - The tags that you want to add to the specified resources. A tag consists of a key and a value that you define.
mkTagResources ::
  -- | 'resourceARNList'
  Lude.NonEmpty Lude.Text ->
  TagResources
mkTagResources pResourceARNList_ =
  TagResources'
    { resourceARNList = pResourceARNList_,
      tags = Lude.mempty
    }

-- | A list of ARNs. An ARN (Amazon Resource Name) uniquely identifies a resource. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'resourceARNList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trResourceARNList :: Lens.Lens' TagResources (Lude.NonEmpty Lude.Text)
trResourceARNList = Lens.lens (resourceARNList :: TagResources -> Lude.NonEmpty Lude.Text) (\s a -> s {resourceARNList = a} :: TagResources)
{-# DEPRECATED trResourceARNList "Use generic-lens or generic-optics with 'resourceARNList' instead." #-}

-- | The tags that you want to add to the specified resources. A tag consists of a key and a value that you define.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTags :: Lens.Lens' TagResources (Lude.HashMap Lude.Text (Lude.Text))
trTags = Lens.lens (tags :: TagResources -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {tags = a} :: TagResources)
{-# DEPRECATED trTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest TagResources where
  type Rs TagResources = TagResourcesResponse
  request = Req.postJSON resourceGroupsTaggingService
  response =
    Res.receiveJSON
      ( \s h x ->
          TagResourcesResponse'
            Lude.<$> (x Lude..?> "FailedResourcesMap" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TagResources where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "ResourceGroupsTaggingAPI_20170126.TagResources" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TagResources where
  toJSON TagResources' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceARNList" Lude..= resourceARNList),
            Lude.Just ("Tags" Lude..= tags)
          ]
      )

instance Lude.ToPath TagResources where
  toPath = Lude.const "/"

instance Lude.ToQuery TagResources where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTagResourcesResponse' smart constructor.
data TagResourcesResponse = TagResourcesResponse'
  { -- | A map containing a key-value pair for each failed item that couldn't be tagged. The key is the ARN of the failed resource. The value is a @FailureInfo@ object that contains an error code, a status code, and an error message. If there are no errors, the @FailedResourcesMap@ is empty.
    failedResourcesMap :: Lude.Maybe (Lude.HashMap Lude.Text (FailureInfo)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagResourcesResponse' with the minimum fields required to make a request.
--
-- * 'failedResourcesMap' - A map containing a key-value pair for each failed item that couldn't be tagged. The key is the ARN of the failed resource. The value is a @FailureInfo@ object that contains an error code, a status code, and an error message. If there are no errors, the @FailedResourcesMap@ is empty.
-- * 'responseStatus' - The response status code.
mkTagResourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TagResourcesResponse
mkTagResourcesResponse pResponseStatus_ =
  TagResourcesResponse'
    { failedResourcesMap = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A map containing a key-value pair for each failed item that couldn't be tagged. The key is the ARN of the failed resource. The value is a @FailureInfo@ object that contains an error code, a status code, and an error message. If there are no errors, the @FailedResourcesMap@ is empty.
--
-- /Note:/ Consider using 'failedResourcesMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trrsFailedResourcesMap :: Lens.Lens' TagResourcesResponse (Lude.Maybe (Lude.HashMap Lude.Text (FailureInfo)))
trrsFailedResourcesMap = Lens.lens (failedResourcesMap :: TagResourcesResponse -> Lude.Maybe (Lude.HashMap Lude.Text (FailureInfo))) (\s a -> s {failedResourcesMap = a} :: TagResourcesResponse)
{-# DEPRECATED trrsFailedResourcesMap "Use generic-lens or generic-optics with 'failedResourcesMap' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trrsResponseStatus :: Lens.Lens' TagResourcesResponse Lude.Int
trrsResponseStatus = Lens.lens (responseStatus :: TagResourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TagResourcesResponse)
{-# DEPRECATED trrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

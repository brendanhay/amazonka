{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.RemoveTagsFromOnPremisesInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from one or more on-premises instances.
module Network.AWS.CodeDeploy.RemoveTagsFromOnPremisesInstances
  ( -- * Creating a request
    RemoveTagsFromOnPremisesInstances (..),
    mkRemoveTagsFromOnPremisesInstances,

    -- ** Request lenses
    rtfopiInstanceNames,
    rtfopiTags,

    -- * Destructuring the response
    RemoveTagsFromOnPremisesInstancesResponse (..),
    mkRemoveTagsFromOnPremisesInstancesResponse,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @RemoveTagsFromOnPremisesInstances@ operation.
--
-- /See:/ 'mkRemoveTagsFromOnPremisesInstances' smart constructor.
data RemoveTagsFromOnPremisesInstances = RemoveTagsFromOnPremisesInstances'
  { -- | The names of the on-premises instances from which to remove tags.
    instanceNames :: [Lude.Text],
    -- | The tag key-value pairs to remove from the on-premises instances.
    tags :: [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTagsFromOnPremisesInstances' with the minimum fields required to make a request.
--
-- * 'instanceNames' - The names of the on-premises instances from which to remove tags.
-- * 'tags' - The tag key-value pairs to remove from the on-premises instances.
mkRemoveTagsFromOnPremisesInstances ::
  RemoveTagsFromOnPremisesInstances
mkRemoveTagsFromOnPremisesInstances =
  RemoveTagsFromOnPremisesInstances'
    { instanceNames = Lude.mempty,
      tags = Lude.mempty
    }

-- | The names of the on-premises instances from which to remove tags.
--
-- /Note:/ Consider using 'instanceNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfopiInstanceNames :: Lens.Lens' RemoveTagsFromOnPremisesInstances [Lude.Text]
rtfopiInstanceNames = Lens.lens (instanceNames :: RemoveTagsFromOnPremisesInstances -> [Lude.Text]) (\s a -> s {instanceNames = a} :: RemoveTagsFromOnPremisesInstances)
{-# DEPRECATED rtfopiInstanceNames "Use generic-lens or generic-optics with 'instanceNames' instead." #-}

-- | The tag key-value pairs to remove from the on-premises instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfopiTags :: Lens.Lens' RemoveTagsFromOnPremisesInstances [Tag]
rtfopiTags = Lens.lens (tags :: RemoveTagsFromOnPremisesInstances -> [Tag]) (\s a -> s {tags = a} :: RemoveTagsFromOnPremisesInstances)
{-# DEPRECATED rtfopiTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest RemoveTagsFromOnPremisesInstances where
  type
    Rs RemoveTagsFromOnPremisesInstances =
      RemoveTagsFromOnPremisesInstancesResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveNull RemoveTagsFromOnPremisesInstancesResponse'

instance Lude.ToHeaders RemoveTagsFromOnPremisesInstances where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeDeploy_20141006.RemoveTagsFromOnPremisesInstances" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RemoveTagsFromOnPremisesInstances where
  toJSON RemoveTagsFromOnPremisesInstances' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("instanceNames" Lude..= instanceNames),
            Lude.Just ("tags" Lude..= tags)
          ]
      )

instance Lude.ToPath RemoveTagsFromOnPremisesInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveTagsFromOnPremisesInstances where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRemoveTagsFromOnPremisesInstancesResponse' smart constructor.
data RemoveTagsFromOnPremisesInstancesResponse = RemoveTagsFromOnPremisesInstancesResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTagsFromOnPremisesInstancesResponse' with the minimum fields required to make a request.
mkRemoveTagsFromOnPremisesInstancesResponse ::
  RemoveTagsFromOnPremisesInstancesResponse
mkRemoveTagsFromOnPremisesInstancesResponse =
  RemoveTagsFromOnPremisesInstancesResponse'

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.AddTagsToOnPremisesInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to on-premises instances.
module Network.AWS.CodeDeploy.AddTagsToOnPremisesInstances
  ( -- * Creating a request
    AddTagsToOnPremisesInstances (..),
    mkAddTagsToOnPremisesInstances,

    -- ** Request lenses
    attopiInstanceNames,
    attopiTags,

    -- * Destructuring the response
    AddTagsToOnPremisesInstancesResponse (..),
    mkAddTagsToOnPremisesInstancesResponse,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of, and adds tags to, an on-premises instance operation.
--
-- /See:/ 'mkAddTagsToOnPremisesInstances' smart constructor.
data AddTagsToOnPremisesInstances = AddTagsToOnPremisesInstances'
  { -- | The names of the on-premises instances to which to add tags.
    instanceNames :: [Lude.Text],
    -- | The tag key-value pairs to add to the on-premises instances.
    --
    -- Keys and values are both required. Keys cannot be null or empty strings. Value-only tags are not allowed.
    tags :: [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTagsToOnPremisesInstances' with the minimum fields required to make a request.
--
-- * 'instanceNames' - The names of the on-premises instances to which to add tags.
-- * 'tags' - The tag key-value pairs to add to the on-premises instances.
--
-- Keys and values are both required. Keys cannot be null or empty strings. Value-only tags are not allowed.
mkAddTagsToOnPremisesInstances ::
  AddTagsToOnPremisesInstances
mkAddTagsToOnPremisesInstances =
  AddTagsToOnPremisesInstances'
    { instanceNames = Lude.mempty,
      tags = Lude.mempty
    }

-- | The names of the on-premises instances to which to add tags.
--
-- /Note:/ Consider using 'instanceNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attopiInstanceNames :: Lens.Lens' AddTagsToOnPremisesInstances [Lude.Text]
attopiInstanceNames = Lens.lens (instanceNames :: AddTagsToOnPremisesInstances -> [Lude.Text]) (\s a -> s {instanceNames = a} :: AddTagsToOnPremisesInstances)
{-# DEPRECATED attopiInstanceNames "Use generic-lens or generic-optics with 'instanceNames' instead." #-}

-- | The tag key-value pairs to add to the on-premises instances.
--
-- Keys and values are both required. Keys cannot be null or empty strings. Value-only tags are not allowed.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attopiTags :: Lens.Lens' AddTagsToOnPremisesInstances [Tag]
attopiTags = Lens.lens (tags :: AddTagsToOnPremisesInstances -> [Tag]) (\s a -> s {tags = a} :: AddTagsToOnPremisesInstances)
{-# DEPRECATED attopiTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest AddTagsToOnPremisesInstances where
  type
    Rs AddTagsToOnPremisesInstances =
      AddTagsToOnPremisesInstancesResponse
  request = Req.postJSON codeDeployService
  response = Res.receiveNull AddTagsToOnPremisesInstancesResponse'

instance Lude.ToHeaders AddTagsToOnPremisesInstances where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeDeploy_20141006.AddTagsToOnPremisesInstances" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddTagsToOnPremisesInstances where
  toJSON AddTagsToOnPremisesInstances' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("instanceNames" Lude..= instanceNames),
            Lude.Just ("tags" Lude..= tags)
          ]
      )

instance Lude.ToPath AddTagsToOnPremisesInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery AddTagsToOnPremisesInstances where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAddTagsToOnPremisesInstancesResponse' smart constructor.
data AddTagsToOnPremisesInstancesResponse = AddTagsToOnPremisesInstancesResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTagsToOnPremisesInstancesResponse' with the minimum fields required to make a request.
mkAddTagsToOnPremisesInstancesResponse ::
  AddTagsToOnPremisesInstancesResponse
mkAddTagsToOnPremisesInstancesResponse =
  AddTagsToOnPremisesInstancesResponse'

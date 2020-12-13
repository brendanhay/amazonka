{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateInstanceSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of a specific virtual private server, or /instance/ . You can use a snapshot to create a new instance that is based on that snapshot.
--
-- The @create instance snapshot@ operation supports tag-based access control via request tags. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateInstanceSnapshot
  ( -- * Creating a request
    CreateInstanceSnapshot (..),
    mkCreateInstanceSnapshot,

    -- ** Request lenses
    cisInstanceSnapshotName,
    cisInstanceName,
    cisTags,

    -- * Destructuring the response
    CreateInstanceSnapshotResponse (..),
    mkCreateInstanceSnapshotResponse,

    -- ** Response lenses
    cisrsOperations,
    cisrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateInstanceSnapshot' smart constructor.
data CreateInstanceSnapshot = CreateInstanceSnapshot'
  { -- | The name for your new snapshot.
    instanceSnapshotName :: Lude.Text,
    -- | The Lightsail instance on which to base your snapshot.
    instanceName :: Lude.Text,
    -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it's created.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateInstanceSnapshot' with the minimum fields required to make a request.
--
-- * 'instanceSnapshotName' - The name for your new snapshot.
-- * 'instanceName' - The Lightsail instance on which to base your snapshot.
-- * 'tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
mkCreateInstanceSnapshot ::
  -- | 'instanceSnapshotName'
  Lude.Text ->
  -- | 'instanceName'
  Lude.Text ->
  CreateInstanceSnapshot
mkCreateInstanceSnapshot pInstanceSnapshotName_ pInstanceName_ =
  CreateInstanceSnapshot'
    { instanceSnapshotName =
        pInstanceSnapshotName_,
      instanceName = pInstanceName_,
      tags = Lude.Nothing
    }

-- | The name for your new snapshot.
--
-- /Note:/ Consider using 'instanceSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisInstanceSnapshotName :: Lens.Lens' CreateInstanceSnapshot Lude.Text
cisInstanceSnapshotName = Lens.lens (instanceSnapshotName :: CreateInstanceSnapshot -> Lude.Text) (\s a -> s {instanceSnapshotName = a} :: CreateInstanceSnapshot)
{-# DEPRECATED cisInstanceSnapshotName "Use generic-lens or generic-optics with 'instanceSnapshotName' instead." #-}

-- | The Lightsail instance on which to base your snapshot.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisInstanceName :: Lens.Lens' CreateInstanceSnapshot Lude.Text
cisInstanceName = Lens.lens (instanceName :: CreateInstanceSnapshot -> Lude.Text) (\s a -> s {instanceName = a} :: CreateInstanceSnapshot)
{-# DEPRECATED cisInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisTags :: Lens.Lens' CreateInstanceSnapshot (Lude.Maybe [Tag])
cisTags = Lens.lens (tags :: CreateInstanceSnapshot -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateInstanceSnapshot)
{-# DEPRECATED cisTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateInstanceSnapshot where
  type Rs CreateInstanceSnapshot = CreateInstanceSnapshotResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateInstanceSnapshotResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateInstanceSnapshot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.CreateInstanceSnapshot" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateInstanceSnapshot where
  toJSON CreateInstanceSnapshot' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("instanceSnapshotName" Lude..= instanceSnapshotName),
            Lude.Just ("instanceName" Lude..= instanceName),
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateInstanceSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateInstanceSnapshot where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateInstanceSnapshotResponse' smart constructor.
data CreateInstanceSnapshotResponse = CreateInstanceSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateInstanceSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkCreateInstanceSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateInstanceSnapshotResponse
mkCreateInstanceSnapshotResponse pResponseStatus_ =
  CreateInstanceSnapshotResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisrsOperations :: Lens.Lens' CreateInstanceSnapshotResponse (Lude.Maybe [Operation])
cisrsOperations = Lens.lens (operations :: CreateInstanceSnapshotResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: CreateInstanceSnapshotResponse)
{-# DEPRECATED cisrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisrsResponseStatus :: Lens.Lens' CreateInstanceSnapshotResponse Lude.Int
cisrsResponseStatus = Lens.lens (responseStatus :: CreateInstanceSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateInstanceSnapshotResponse)
{-# DEPRECATED cisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

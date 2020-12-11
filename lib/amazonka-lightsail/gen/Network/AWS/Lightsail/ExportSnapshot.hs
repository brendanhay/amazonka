{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.ExportSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports an Amazon Lightsail instance or block storage disk snapshot to Amazon Elastic Compute Cloud (Amazon EC2). This operation results in an export snapshot record that can be used with the @create cloud formation stack@ operation to create new Amazon EC2 instances.
--
-- Exported instance snapshots appear in Amazon EC2 as Amazon Machine Images (AMIs), and the instance system disk appears as an Amazon Elastic Block Store (Amazon EBS) volume. Exported disk snapshots appear in Amazon EC2 as Amazon EBS volumes. Snapshots are exported to the same Amazon Web Services Region in Amazon EC2 as the source Lightsail snapshot.
--
-- The @export snapshot@ operation supports tag-based access control via resource tags applied to the resource identified by @source snapshot name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.ExportSnapshot
  ( -- * Creating a request
    ExportSnapshot (..),
    mkExportSnapshot,

    -- ** Request lenses
    esSourceSnapshotName,

    -- * Destructuring the response
    ExportSnapshotResponse (..),
    mkExportSnapshotResponse,

    -- ** Response lenses
    esrsOperations,
    esrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkExportSnapshot' smart constructor.
newtype ExportSnapshot = ExportSnapshot'
  { sourceSnapshotName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportSnapshot' with the minimum fields required to make a request.
--
-- * 'sourceSnapshotName' - The name of the instance or disk snapshot to be exported to Amazon EC2.
mkExportSnapshot ::
  -- | 'sourceSnapshotName'
  Lude.Text ->
  ExportSnapshot
mkExportSnapshot pSourceSnapshotName_ =
  ExportSnapshot' {sourceSnapshotName = pSourceSnapshotName_}

-- | The name of the instance or disk snapshot to be exported to Amazon EC2.
--
-- /Note:/ Consider using 'sourceSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSourceSnapshotName :: Lens.Lens' ExportSnapshot Lude.Text
esSourceSnapshotName = Lens.lens (sourceSnapshotName :: ExportSnapshot -> Lude.Text) (\s a -> s {sourceSnapshotName = a} :: ExportSnapshot)
{-# DEPRECATED esSourceSnapshotName "Use generic-lens or generic-optics with 'sourceSnapshotName' instead." #-}

instance Lude.AWSRequest ExportSnapshot where
  type Rs ExportSnapshot = ExportSnapshotResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          ExportSnapshotResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ExportSnapshot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.ExportSnapshot" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ExportSnapshot where
  toJSON ExportSnapshot' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("sourceSnapshotName" Lude..= sourceSnapshotName)]
      )

instance Lude.ToPath ExportSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery ExportSnapshot where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkExportSnapshotResponse' smart constructor.
data ExportSnapshotResponse = ExportSnapshotResponse'
  { operations ::
      Lude.Maybe [Operation],
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

-- | Creates a value of 'ExportSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkExportSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ExportSnapshotResponse
mkExportSnapshotResponse pResponseStatus_ =
  ExportSnapshotResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrsOperations :: Lens.Lens' ExportSnapshotResponse (Lude.Maybe [Operation])
esrsOperations = Lens.lens (operations :: ExportSnapshotResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: ExportSnapshotResponse)
{-# DEPRECATED esrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrsResponseStatus :: Lens.Lens' ExportSnapshotResponse Lude.Int
esrsResponseStatus = Lens.lens (responseStatus :: ExportSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ExportSnapshotResponse)
{-# DEPRECATED esrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeExport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing table export.
module Network.AWS.DynamoDB.DescribeExport
  ( -- * Creating a request
    DescribeExport (..),
    mkDescribeExport,

    -- ** Request lenses
    deExportARN,

    -- * Destructuring the response
    DescribeExportResponse (..),
    mkDescribeExportResponse,

    -- ** Response lenses
    defrsExportDescription,
    defrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeExport' smart constructor.
newtype DescribeExport = DescribeExport'
  { -- | The Amazon Resource Name (ARN) associated with the export.
    exportARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeExport' with the minimum fields required to make a request.
--
-- * 'exportARN' - The Amazon Resource Name (ARN) associated with the export.
mkDescribeExport ::
  -- | 'exportARN'
  Lude.Text ->
  DescribeExport
mkDescribeExport pExportARN_ =
  DescribeExport' {exportARN = pExportARN_}

-- | The Amazon Resource Name (ARN) associated with the export.
--
-- /Note:/ Consider using 'exportARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deExportARN :: Lens.Lens' DescribeExport Lude.Text
deExportARN = Lens.lens (exportARN :: DescribeExport -> Lude.Text) (\s a -> s {exportARN = a} :: DescribeExport)
{-# DEPRECATED deExportARN "Use generic-lens or generic-optics with 'exportARN' instead." #-}

instance Lude.AWSRequest DescribeExport where
  type Rs DescribeExport = DescribeExportResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeExportResponse'
            Lude.<$> (x Lude..?> "ExportDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeExport where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.DescribeExport" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeExport where
  toJSON DescribeExport' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ExportArn" Lude..= exportARN)])

instance Lude.ToPath DescribeExport where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeExport where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeExportResponse' smart constructor.
data DescribeExportResponse = DescribeExportResponse'
  { -- | Represents the properties of the export.
    exportDescription :: Lude.Maybe ExportDescription,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeExportResponse' with the minimum fields required to make a request.
--
-- * 'exportDescription' - Represents the properties of the export.
-- * 'responseStatus' - The response status code.
mkDescribeExportResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeExportResponse
mkDescribeExportResponse pResponseStatus_ =
  DescribeExportResponse'
    { exportDescription = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Represents the properties of the export.
--
-- /Note:/ Consider using 'exportDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defrsExportDescription :: Lens.Lens' DescribeExportResponse (Lude.Maybe ExportDescription)
defrsExportDescription = Lens.lens (exportDescription :: DescribeExportResponse -> Lude.Maybe ExportDescription) (\s a -> s {exportDescription = a} :: DescribeExportResponse)
{-# DEPRECATED defrsExportDescription "Use generic-lens or generic-optics with 'exportDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defrsResponseStatus :: Lens.Lens' DescribeExportResponse Lude.Int
defrsResponseStatus = Lens.lens (responseStatus :: DescribeExportResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeExportResponse)
{-# DEPRECATED defrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

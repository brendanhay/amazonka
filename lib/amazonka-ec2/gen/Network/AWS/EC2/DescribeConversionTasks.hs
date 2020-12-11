{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeConversionTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified conversion tasks or all your conversion tasks. For more information, see the <https://docs.aws.amazon.com/vm-import/latest/userguide/ VM Import/Export User Guide> .
--
-- For information about the import manifest referenced by this API action, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
module Network.AWS.EC2.DescribeConversionTasks
  ( -- * Creating a request
    DescribeConversionTasks (..),
    mkDescribeConversionTasks,

    -- ** Request lenses
    dctConversionTaskIds,
    dctDryRun,

    -- * Destructuring the response
    DescribeConversionTasksResponse (..),
    mkDescribeConversionTasksResponse,

    -- ** Response lenses
    dctrsConversionTasks,
    dctrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeConversionTasks' smart constructor.
data DescribeConversionTasks = DescribeConversionTasks'
  { conversionTaskIds ::
      Lude.Maybe [Lude.Text],
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConversionTasks' with the minimum fields required to make a request.
--
-- * 'conversionTaskIds' - The conversion task IDs.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDescribeConversionTasks ::
  DescribeConversionTasks
mkDescribeConversionTasks =
  DescribeConversionTasks'
    { conversionTaskIds = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The conversion task IDs.
--
-- /Note:/ Consider using 'conversionTaskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctConversionTaskIds :: Lens.Lens' DescribeConversionTasks (Lude.Maybe [Lude.Text])
dctConversionTaskIds = Lens.lens (conversionTaskIds :: DescribeConversionTasks -> Lude.Maybe [Lude.Text]) (\s a -> s {conversionTaskIds = a} :: DescribeConversionTasks)
{-# DEPRECATED dctConversionTaskIds "Use generic-lens or generic-optics with 'conversionTaskIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctDryRun :: Lens.Lens' DescribeConversionTasks (Lude.Maybe Lude.Bool)
dctDryRun = Lens.lens (dryRun :: DescribeConversionTasks -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeConversionTasks)
{-# DEPRECATED dctDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DescribeConversionTasks where
  type Rs DescribeConversionTasks = DescribeConversionTasksResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeConversionTasksResponse'
            Lude.<$> ( x Lude..@? "conversionTasks" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeConversionTasks where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeConversionTasks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeConversionTasks where
  toQuery DescribeConversionTasks' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeConversionTasks" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "ConversionTaskId" Lude.<$> conversionTaskIds),
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDescribeConversionTasksResponse' smart constructor.
data DescribeConversionTasksResponse = DescribeConversionTasksResponse'
  { conversionTasks ::
      Lude.Maybe [ConversionTask],
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

-- | Creates a value of 'DescribeConversionTasksResponse' with the minimum fields required to make a request.
--
-- * 'conversionTasks' - Information about the conversion tasks.
-- * 'responseStatus' - The response status code.
mkDescribeConversionTasksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeConversionTasksResponse
mkDescribeConversionTasksResponse pResponseStatus_ =
  DescribeConversionTasksResponse'
    { conversionTasks = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the conversion tasks.
--
-- /Note:/ Consider using 'conversionTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctrsConversionTasks :: Lens.Lens' DescribeConversionTasksResponse (Lude.Maybe [ConversionTask])
dctrsConversionTasks = Lens.lens (conversionTasks :: DescribeConversionTasksResponse -> Lude.Maybe [ConversionTask]) (\s a -> s {conversionTasks = a} :: DescribeConversionTasksResponse)
{-# DEPRECATED dctrsConversionTasks "Use generic-lens or generic-optics with 'conversionTasks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctrsResponseStatus :: Lens.Lens' DescribeConversionTasksResponse Lude.Int
dctrsResponseStatus = Lens.lens (responseStatus :: DescribeConversionTasksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeConversionTasksResponse)
{-# DEPRECATED dctrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

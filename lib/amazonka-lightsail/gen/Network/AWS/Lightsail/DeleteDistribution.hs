{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes your Amazon Lightsail content delivery network (CDN) distribution.
module Network.AWS.Lightsail.DeleteDistribution
  ( -- * Creating a request
    DeleteDistribution (..),
    mkDeleteDistribution,

    -- ** Request lenses
    ddDistributionName,

    -- * Destructuring the response
    DeleteDistributionResponse (..),
    mkDeleteDistributionResponse,

    -- ** Response lenses
    dddrsOperation,
    dddrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDistribution' smart constructor.
newtype DeleteDistribution = DeleteDistribution'
  { distributionName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDistribution' with the minimum fields required to make a request.
--
-- * 'distributionName' - The name of the distribution to delete.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
mkDeleteDistribution ::
  DeleteDistribution
mkDeleteDistribution =
  DeleteDistribution' {distributionName = Lude.Nothing}

-- | The name of the distribution to delete.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
--
-- /Note:/ Consider using 'distributionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDistributionName :: Lens.Lens' DeleteDistribution (Lude.Maybe Lude.Text)
ddDistributionName = Lens.lens (distributionName :: DeleteDistribution -> Lude.Maybe Lude.Text) (\s a -> s {distributionName = a} :: DeleteDistribution)
{-# DEPRECATED ddDistributionName "Use generic-lens or generic-optics with 'distributionName' instead." #-}

instance Lude.AWSRequest DeleteDistribution where
  type Rs DeleteDistribution = DeleteDistributionResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteDistributionResponse'
            Lude.<$> (x Lude..?> "operation") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDistribution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.DeleteDistribution" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDistribution where
  toJSON DeleteDistribution' {..} =
    Lude.object
      ( Lude.catMaybes
          [("distributionName" Lude..=) Lude.<$> distributionName]
      )

instance Lude.ToPath DeleteDistribution where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDistribution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDistributionResponse' smart constructor.
data DeleteDistributionResponse = DeleteDistributionResponse'
  { operation ::
      Lude.Maybe Operation,
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

-- | Creates a value of 'DeleteDistributionResponse' with the minimum fields required to make a request.
--
-- * 'operation' - An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkDeleteDistributionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDistributionResponse
mkDeleteDistributionResponse pResponseStatus_ =
  DeleteDistributionResponse'
    { operation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dddrsOperation :: Lens.Lens' DeleteDistributionResponse (Lude.Maybe Operation)
dddrsOperation = Lens.lens (operation :: DeleteDistributionResponse -> Lude.Maybe Operation) (\s a -> s {operation = a} :: DeleteDistributionResponse)
{-# DEPRECATED dddrsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dddrsResponseStatus :: Lens.Lens' DeleteDistributionResponse Lude.Int
dddrsResponseStatus = Lens.lens (responseStatus :: DeleteDistributionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDistributionResponse)
{-# DEPRECATED dddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.DeleteVPCEConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a configuration for your Amazon Virtual Private Cloud (VPC) endpoint.
module Network.AWS.DeviceFarm.DeleteVPCEConfiguration
  ( -- * Creating a request
    DeleteVPCEConfiguration (..),
    mkDeleteVPCEConfiguration,

    -- ** Request lenses
    dvecArn,

    -- * Destructuring the response
    DeleteVPCEConfigurationResponse (..),
    mkDeleteVPCEConfigurationResponse,

    -- ** Response lenses
    dvecrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteVPCEConfiguration' smart constructor.
newtype DeleteVPCEConfiguration = DeleteVPCEConfiguration'
  { -- | The Amazon Resource Name (ARN) of the VPC endpoint configuration you want to delete.
    arn :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVPCEConfiguration' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the VPC endpoint configuration you want to delete.
mkDeleteVPCEConfiguration ::
  -- | 'arn'
  Lude.Text ->
  DeleteVPCEConfiguration
mkDeleteVPCEConfiguration pArn_ =
  DeleteVPCEConfiguration' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the VPC endpoint configuration you want to delete.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecArn :: Lens.Lens' DeleteVPCEConfiguration Lude.Text
dvecArn = Lens.lens (arn :: DeleteVPCEConfiguration -> Lude.Text) (\s a -> s {arn = a} :: DeleteVPCEConfiguration)
{-# DEPRECATED dvecArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest DeleteVPCEConfiguration where
  type Rs DeleteVPCEConfiguration = DeleteVPCEConfigurationResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteVPCEConfigurationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteVPCEConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.DeleteVPCEConfiguration" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteVPCEConfiguration where
  toJSON DeleteVPCEConfiguration' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath DeleteVPCEConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteVPCEConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteVPCEConfigurationResponse' smart constructor.
newtype DeleteVPCEConfigurationResponse = DeleteVPCEConfigurationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVPCEConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteVPCEConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteVPCEConfigurationResponse
mkDeleteVPCEConfigurationResponse pResponseStatus_ =
  DeleteVPCEConfigurationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecrsResponseStatus :: Lens.Lens' DeleteVPCEConfigurationResponse Lude.Int
dvecrsResponseStatus = Lens.lens (responseStatus :: DeleteVPCEConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteVPCEConfigurationResponse)
{-# DEPRECATED dvecrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteDeviceUsageData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When this action is called for a specified shared device, it allows authorized users to delete the device's entire previous history of voice input data and associated response data. This action can be called once every 24 hours for a specific shared device.
module Network.AWS.AlexaBusiness.DeleteDeviceUsageData
  ( -- * Creating a request
    DeleteDeviceUsageData (..),
    mkDeleteDeviceUsageData,

    -- ** Request lenses
    ddudDeviceUsageType,
    ddudDeviceARN,

    -- * Destructuring the response
    DeleteDeviceUsageDataResponse (..),
    mkDeleteDeviceUsageDataResponse,

    -- ** Response lenses
    ddudrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDeviceUsageData' smart constructor.
data DeleteDeviceUsageData = DeleteDeviceUsageData'
  { -- | The type of usage data to delete.
    deviceUsageType :: DeviceUsageType,
    -- | The ARN of the device.
    deviceARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDeviceUsageData' with the minimum fields required to make a request.
--
-- * 'deviceUsageType' - The type of usage data to delete.
-- * 'deviceARN' - The ARN of the device.
mkDeleteDeviceUsageData ::
  -- | 'deviceUsageType'
  DeviceUsageType ->
  -- | 'deviceARN'
  Lude.Text ->
  DeleteDeviceUsageData
mkDeleteDeviceUsageData pDeviceUsageType_ pDeviceARN_ =
  DeleteDeviceUsageData'
    { deviceUsageType = pDeviceUsageType_,
      deviceARN = pDeviceARN_
    }

-- | The type of usage data to delete.
--
-- /Note:/ Consider using 'deviceUsageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddudDeviceUsageType :: Lens.Lens' DeleteDeviceUsageData DeviceUsageType
ddudDeviceUsageType = Lens.lens (deviceUsageType :: DeleteDeviceUsageData -> DeviceUsageType) (\s a -> s {deviceUsageType = a} :: DeleteDeviceUsageData)
{-# DEPRECATED ddudDeviceUsageType "Use generic-lens or generic-optics with 'deviceUsageType' instead." #-}

-- | The ARN of the device.
--
-- /Note:/ Consider using 'deviceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddudDeviceARN :: Lens.Lens' DeleteDeviceUsageData Lude.Text
ddudDeviceARN = Lens.lens (deviceARN :: DeleteDeviceUsageData -> Lude.Text) (\s a -> s {deviceARN = a} :: DeleteDeviceUsageData)
{-# DEPRECATED ddudDeviceARN "Use generic-lens or generic-optics with 'deviceARN' instead." #-}

instance Lude.AWSRequest DeleteDeviceUsageData where
  type Rs DeleteDeviceUsageData = DeleteDeviceUsageDataResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteDeviceUsageDataResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDeviceUsageData where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.DeleteDeviceUsageData" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDeviceUsageData where
  toJSON DeleteDeviceUsageData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DeviceUsageType" Lude..= deviceUsageType),
            Lude.Just ("DeviceArn" Lude..= deviceARN)
          ]
      )

instance Lude.ToPath DeleteDeviceUsageData where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDeviceUsageData where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDeviceUsageDataResponse' smart constructor.
newtype DeleteDeviceUsageDataResponse = DeleteDeviceUsageDataResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDeviceUsageDataResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteDeviceUsageDataResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDeviceUsageDataResponse
mkDeleteDeviceUsageDataResponse pResponseStatus_ =
  DeleteDeviceUsageDataResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddudrsResponseStatus :: Lens.Lens' DeleteDeviceUsageDataResponse Lude.Int
ddudrsResponseStatus = Lens.lens (responseStatus :: DeleteDeviceUsageDataResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDeviceUsageDataResponse)
{-# DEPRECATED ddudrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

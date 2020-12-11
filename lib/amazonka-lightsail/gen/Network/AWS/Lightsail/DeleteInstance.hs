{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Lightsail instance.
--
-- The @delete instance@ operation supports tag-based access control via resource tags applied to the resource identified by @instance name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteInstance
  ( -- * Creating a request
    DeleteInstance (..),
    mkDeleteInstance,

    -- ** Request lenses
    diForceDeleteAddOns,
    diInstanceName,

    -- * Destructuring the response
    DeleteInstanceResponse (..),
    mkDeleteInstanceResponse,

    -- ** Response lenses
    dirsOperations,
    dirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteInstance' smart constructor.
data DeleteInstance = DeleteInstance'
  { forceDeleteAddOns ::
      Lude.Maybe Lude.Bool,
    instanceName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteInstance' with the minimum fields required to make a request.
--
-- * 'forceDeleteAddOns' - A Boolean value to indicate whether to delete the enabled add-ons for the disk.
-- * 'instanceName' - The name of the instance to delete.
mkDeleteInstance ::
  -- | 'instanceName'
  Lude.Text ->
  DeleteInstance
mkDeleteInstance pInstanceName_ =
  DeleteInstance'
    { forceDeleteAddOns = Lude.Nothing,
      instanceName = pInstanceName_
    }

-- | A Boolean value to indicate whether to delete the enabled add-ons for the disk.
--
-- /Note:/ Consider using 'forceDeleteAddOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diForceDeleteAddOns :: Lens.Lens' DeleteInstance (Lude.Maybe Lude.Bool)
diForceDeleteAddOns = Lens.lens (forceDeleteAddOns :: DeleteInstance -> Lude.Maybe Lude.Bool) (\s a -> s {forceDeleteAddOns = a} :: DeleteInstance)
{-# DEPRECATED diForceDeleteAddOns "Use generic-lens or generic-optics with 'forceDeleteAddOns' instead." #-}

-- | The name of the instance to delete.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInstanceName :: Lens.Lens' DeleteInstance Lude.Text
diInstanceName = Lens.lens (instanceName :: DeleteInstance -> Lude.Text) (\s a -> s {instanceName = a} :: DeleteInstance)
{-# DEPRECATED diInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Lude.AWSRequest DeleteInstance where
  type Rs DeleteInstance = DeleteInstanceResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteInstanceResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.DeleteInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteInstance where
  toJSON DeleteInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("forceDeleteAddOns" Lude..=) Lude.<$> forceDeleteAddOns,
            Lude.Just ("instanceName" Lude..= instanceName)
          ]
      )

instance Lude.ToPath DeleteInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteInstanceResponse' smart constructor.
data DeleteInstanceResponse = DeleteInstanceResponse'
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

-- | Creates a value of 'DeleteInstanceResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkDeleteInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteInstanceResponse
mkDeleteInstanceResponse pResponseStatus_ =
  DeleteInstanceResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsOperations :: Lens.Lens' DeleteInstanceResponse (Lude.Maybe [Operation])
dirsOperations = Lens.lens (operations :: DeleteInstanceResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: DeleteInstanceResponse)
{-# DEPRECATED dirsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsResponseStatus :: Lens.Lens' DeleteInstanceResponse Lude.Int
dirsResponseStatus = Lens.lens (responseStatus :: DeleteInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteInstanceResponse)
{-# DEPRECATED dirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

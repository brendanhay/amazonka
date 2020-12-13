{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.EnableAddOn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or modifies an add-on for an Amazon Lightsail resource. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
module Network.AWS.Lightsail.EnableAddOn
  ( -- * Creating a request
    EnableAddOn (..),
    mkEnableAddOn,

    -- ** Request lenses
    eaoResourceName,
    eaoAddOnRequest,

    -- * Destructuring the response
    EnableAddOnResponse (..),
    mkEnableAddOnResponse,

    -- ** Response lenses
    eaorsOperations,
    eaorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnableAddOn' smart constructor.
data EnableAddOn = EnableAddOn'
  { -- | The name of the source resource for which to enable or modify the add-on.
    resourceName :: Lude.Text,
    -- | An array of strings representing the add-on to enable or modify.
    addOnRequest :: AddOnRequest
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableAddOn' with the minimum fields required to make a request.
--
-- * 'resourceName' - The name of the source resource for which to enable or modify the add-on.
-- * 'addOnRequest' - An array of strings representing the add-on to enable or modify.
mkEnableAddOn ::
  -- | 'resourceName'
  Lude.Text ->
  -- | 'addOnRequest'
  AddOnRequest ->
  EnableAddOn
mkEnableAddOn pResourceName_ pAddOnRequest_ =
  EnableAddOn'
    { resourceName = pResourceName_,
      addOnRequest = pAddOnRequest_
    }

-- | The name of the source resource for which to enable or modify the add-on.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaoResourceName :: Lens.Lens' EnableAddOn Lude.Text
eaoResourceName = Lens.lens (resourceName :: EnableAddOn -> Lude.Text) (\s a -> s {resourceName = a} :: EnableAddOn)
{-# DEPRECATED eaoResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | An array of strings representing the add-on to enable or modify.
--
-- /Note:/ Consider using 'addOnRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaoAddOnRequest :: Lens.Lens' EnableAddOn AddOnRequest
eaoAddOnRequest = Lens.lens (addOnRequest :: EnableAddOn -> AddOnRequest) (\s a -> s {addOnRequest = a} :: EnableAddOn)
{-# DEPRECATED eaoAddOnRequest "Use generic-lens or generic-optics with 'addOnRequest' instead." #-}

instance Lude.AWSRequest EnableAddOn where
  type Rs EnableAddOn = EnableAddOnResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          EnableAddOnResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EnableAddOn where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.EnableAddOn" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON EnableAddOn where
  toJSON EnableAddOn' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("resourceName" Lude..= resourceName),
            Lude.Just ("addOnRequest" Lude..= addOnRequest)
          ]
      )

instance Lude.ToPath EnableAddOn where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableAddOn where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkEnableAddOnResponse' smart constructor.
data EnableAddOnResponse = EnableAddOnResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableAddOnResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkEnableAddOnResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  EnableAddOnResponse
mkEnableAddOnResponse pResponseStatus_ =
  EnableAddOnResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaorsOperations :: Lens.Lens' EnableAddOnResponse (Lude.Maybe [Operation])
eaorsOperations = Lens.lens (operations :: EnableAddOnResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: EnableAddOnResponse)
{-# DEPRECATED eaorsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaorsResponseStatus :: Lens.Lens' EnableAddOnResponse Lude.Int
eaorsResponseStatus = Lens.lens (responseStatus :: EnableAddOnResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EnableAddOnResponse)
{-# DEPRECATED eaorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

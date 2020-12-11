{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DisableAddOn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables an add-on for an Amazon Lightsail resource. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
module Network.AWS.Lightsail.DisableAddOn
  ( -- * Creating a request
    DisableAddOn (..),
    mkDisableAddOn,

    -- ** Request lenses
    daoAddOnType,
    daoResourceName,

    -- * Destructuring the response
    DisableAddOnResponse (..),
    mkDisableAddOnResponse,

    -- ** Response lenses
    daorsOperations,
    daorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisableAddOn' smart constructor.
data DisableAddOn = DisableAddOn'
  { addOnType :: AddOnType,
    resourceName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableAddOn' with the minimum fields required to make a request.
--
-- * 'addOnType' - The add-on type to disable.
-- * 'resourceName' - The name of the source resource for which to disable the add-on.
mkDisableAddOn ::
  -- | 'addOnType'
  AddOnType ->
  -- | 'resourceName'
  Lude.Text ->
  DisableAddOn
mkDisableAddOn pAddOnType_ pResourceName_ =
  DisableAddOn'
    { addOnType = pAddOnType_,
      resourceName = pResourceName_
    }

-- | The add-on type to disable.
--
-- /Note:/ Consider using 'addOnType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoAddOnType :: Lens.Lens' DisableAddOn AddOnType
daoAddOnType = Lens.lens (addOnType :: DisableAddOn -> AddOnType) (\s a -> s {addOnType = a} :: DisableAddOn)
{-# DEPRECATED daoAddOnType "Use generic-lens or generic-optics with 'addOnType' instead." #-}

-- | The name of the source resource for which to disable the add-on.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoResourceName :: Lens.Lens' DisableAddOn Lude.Text
daoResourceName = Lens.lens (resourceName :: DisableAddOn -> Lude.Text) (\s a -> s {resourceName = a} :: DisableAddOn)
{-# DEPRECATED daoResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

instance Lude.AWSRequest DisableAddOn where
  type Rs DisableAddOn = DisableAddOnResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DisableAddOnResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisableAddOn where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.DisableAddOn" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisableAddOn where
  toJSON DisableAddOn' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("addOnType" Lude..= addOnType),
            Lude.Just ("resourceName" Lude..= resourceName)
          ]
      )

instance Lude.ToPath DisableAddOn where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableAddOn where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisableAddOnResponse' smart constructor.
data DisableAddOnResponse = DisableAddOnResponse'
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

-- | Creates a value of 'DisableAddOnResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkDisableAddOnResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisableAddOnResponse
mkDisableAddOnResponse pResponseStatus_ =
  DisableAddOnResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daorsOperations :: Lens.Lens' DisableAddOnResponse (Lude.Maybe [Operation])
daorsOperations = Lens.lens (operations :: DisableAddOnResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: DisableAddOnResponse)
{-# DEPRECATED daorsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daorsResponseStatus :: Lens.Lens' DisableAddOnResponse Lude.Int
daorsResponseStatus = Lens.lens (responseStatus :: DisableAddOnResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisableAddOnResponse)
{-# DEPRECATED daorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

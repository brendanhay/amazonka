{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.AssociateSecurityKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a security key to the instance.
module Network.AWS.Connect.AssociateSecurityKey
  ( -- * Creating a request
    AssociateSecurityKey (..),
    mkAssociateSecurityKey,

    -- ** Request lenses
    askInstanceId,
    askKey,

    -- * Destructuring the response
    AssociateSecurityKeyResponse (..),
    mkAssociateSecurityKeyResponse,

    -- ** Response lenses
    askrsAssociationId,
    askrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateSecurityKey' smart constructor.
data AssociateSecurityKey = AssociateSecurityKey'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | A valid security key in PEM format.
    key :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateSecurityKey' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'key' - A valid security key in PEM format.
mkAssociateSecurityKey ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'key'
  Lude.Text ->
  AssociateSecurityKey
mkAssociateSecurityKey pInstanceId_ pKey_ =
  AssociateSecurityKey' {instanceId = pInstanceId_, key = pKey_}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
askInstanceId :: Lens.Lens' AssociateSecurityKey Lude.Text
askInstanceId = Lens.lens (instanceId :: AssociateSecurityKey -> Lude.Text) (\s a -> s {instanceId = a} :: AssociateSecurityKey)
{-# DEPRECATED askInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | A valid security key in PEM format.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
askKey :: Lens.Lens' AssociateSecurityKey Lude.Text
askKey = Lens.lens (key :: AssociateSecurityKey -> Lude.Text) (\s a -> s {key = a} :: AssociateSecurityKey)
{-# DEPRECATED askKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.AWSRequest AssociateSecurityKey where
  type Rs AssociateSecurityKey = AssociateSecurityKeyResponse
  request = Req.putJSON connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          AssociateSecurityKeyResponse'
            Lude.<$> (x Lude..?> "AssociationId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateSecurityKey where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateSecurityKey where
  toJSON AssociateSecurityKey' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Key" Lude..= key)])

instance Lude.ToPath AssociateSecurityKey where
  toPath AssociateSecurityKey' {..} =
    Lude.mconcat
      ["/instance/", Lude.toBS instanceId, "/security-key"]

instance Lude.ToQuery AssociateSecurityKey where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateSecurityKeyResponse' smart constructor.
data AssociateSecurityKeyResponse = AssociateSecurityKeyResponse'
  { -- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
    associationId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateSecurityKeyResponse' with the minimum fields required to make a request.
--
-- * 'associationId' - The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
-- * 'responseStatus' - The response status code.
mkAssociateSecurityKeyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateSecurityKeyResponse
mkAssociateSecurityKeyResponse pResponseStatus_ =
  AssociateSecurityKeyResponse'
    { associationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
askrsAssociationId :: Lens.Lens' AssociateSecurityKeyResponse (Lude.Maybe Lude.Text)
askrsAssociationId = Lens.lens (associationId :: AssociateSecurityKeyResponse -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: AssociateSecurityKeyResponse)
{-# DEPRECATED askrsAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
askrsResponseStatus :: Lens.Lens' AssociateSecurityKeyResponse Lude.Int
askrsResponseStatus = Lens.lens (responseStatus :: AssociateSecurityKeyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateSecurityKeyResponse)
{-# DEPRECATED askrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

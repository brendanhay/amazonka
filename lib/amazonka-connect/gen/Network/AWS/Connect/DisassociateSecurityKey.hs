{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DisassociateSecurityKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified security key.
module Network.AWS.Connect.DisassociateSecurityKey
  ( -- * Creating a request
    DisassociateSecurityKey (..),
    mkDisassociateSecurityKey,

    -- ** Request lenses
    dskInstanceId,
    dskAssociationId,

    -- * Destructuring the response
    DisassociateSecurityKeyResponse (..),
    mkDisassociateSecurityKeyResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateSecurityKey' smart constructor.
data DisassociateSecurityKey = DisassociateSecurityKey'
  { instanceId ::
      Lude.Text,
    associationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateSecurityKey' with the minimum fields required to make a request.
--
-- * 'associationId' - The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
-- * 'instanceId' - The identifier of the Amazon Connect instance.
mkDisassociateSecurityKey ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'associationId'
  Lude.Text ->
  DisassociateSecurityKey
mkDisassociateSecurityKey pInstanceId_ pAssociationId_ =
  DisassociateSecurityKey'
    { instanceId = pInstanceId_,
      associationId = pAssociationId_
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dskInstanceId :: Lens.Lens' DisassociateSecurityKey Lude.Text
dskInstanceId = Lens.lens (instanceId :: DisassociateSecurityKey -> Lude.Text) (\s a -> s {instanceId = a} :: DisassociateSecurityKey)
{-# DEPRECATED dskInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dskAssociationId :: Lens.Lens' DisassociateSecurityKey Lude.Text
dskAssociationId = Lens.lens (associationId :: DisassociateSecurityKey -> Lude.Text) (\s a -> s {associationId = a} :: DisassociateSecurityKey)
{-# DEPRECATED dskAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

instance Lude.AWSRequest DisassociateSecurityKey where
  type Rs DisassociateSecurityKey = DisassociateSecurityKeyResponse
  request = Req.delete connectService
  response = Res.receiveNull DisassociateSecurityKeyResponse'

instance Lude.ToHeaders DisassociateSecurityKey where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DisassociateSecurityKey where
  toPath DisassociateSecurityKey' {..} =
    Lude.mconcat
      [ "/instance/",
        Lude.toBS instanceId,
        "/security-key/",
        Lude.toBS associationId
      ]

instance Lude.ToQuery DisassociateSecurityKey where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateSecurityKeyResponse' smart constructor.
data DisassociateSecurityKeyResponse = DisassociateSecurityKeyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateSecurityKeyResponse' with the minimum fields required to make a request.
mkDisassociateSecurityKeyResponse ::
  DisassociateSecurityKeyResponse
mkDisassociateSecurityKeyResponse =
  DisassociateSecurityKeyResponse'

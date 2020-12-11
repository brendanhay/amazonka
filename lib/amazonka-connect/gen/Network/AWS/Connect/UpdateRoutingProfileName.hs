{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateRoutingProfileName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and description of a routing profile. The request accepts the following data in JSON format. At least @Name@ or @Description@ must be provided.
module Network.AWS.Connect.UpdateRoutingProfileName
  ( -- * Creating a request
    UpdateRoutingProfileName (..),
    mkUpdateRoutingProfileName,

    -- ** Request lenses
    urpnName,
    urpnDescription,
    urpnInstanceId,
    urpnRoutingProfileId,

    -- * Destructuring the response
    UpdateRoutingProfileNameResponse (..),
    mkUpdateRoutingProfileNameResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateRoutingProfileName' smart constructor.
data UpdateRoutingProfileName = UpdateRoutingProfileName'
  { name ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    instanceId :: Lude.Text,
    routingProfileId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRoutingProfileName' with the minimum fields required to make a request.
--
-- * 'description' - The description of the routing profile. Must not be more than 250 characters.
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'name' - The name of the routing profile. Must not be more than 127 characters.
-- * 'routingProfileId' - The identifier of the routing profile.
mkUpdateRoutingProfileName ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'routingProfileId'
  Lude.Text ->
  UpdateRoutingProfileName
mkUpdateRoutingProfileName pInstanceId_ pRoutingProfileId_ =
  UpdateRoutingProfileName'
    { name = Lude.Nothing,
      description = Lude.Nothing,
      instanceId = pInstanceId_,
      routingProfileId = pRoutingProfileId_
    }

-- | The name of the routing profile. Must not be more than 127 characters.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpnName :: Lens.Lens' UpdateRoutingProfileName (Lude.Maybe Lude.Text)
urpnName = Lens.lens (name :: UpdateRoutingProfileName -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateRoutingProfileName)
{-# DEPRECATED urpnName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The description of the routing profile. Must not be more than 250 characters.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpnDescription :: Lens.Lens' UpdateRoutingProfileName (Lude.Maybe Lude.Text)
urpnDescription = Lens.lens (description :: UpdateRoutingProfileName -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateRoutingProfileName)
{-# DEPRECATED urpnDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpnInstanceId :: Lens.Lens' UpdateRoutingProfileName Lude.Text
urpnInstanceId = Lens.lens (instanceId :: UpdateRoutingProfileName -> Lude.Text) (\s a -> s {instanceId = a} :: UpdateRoutingProfileName)
{-# DEPRECATED urpnInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the routing profile.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpnRoutingProfileId :: Lens.Lens' UpdateRoutingProfileName Lude.Text
urpnRoutingProfileId = Lens.lens (routingProfileId :: UpdateRoutingProfileName -> Lude.Text) (\s a -> s {routingProfileId = a} :: UpdateRoutingProfileName)
{-# DEPRECATED urpnRoutingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead." #-}

instance Lude.AWSRequest UpdateRoutingProfileName where
  type Rs UpdateRoutingProfileName = UpdateRoutingProfileNameResponse
  request = Req.postJSON connectService
  response = Res.receiveNull UpdateRoutingProfileNameResponse'

instance Lude.ToHeaders UpdateRoutingProfileName where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateRoutingProfileName where
  toJSON UpdateRoutingProfileName' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Name" Lude..=) Lude.<$> name,
            ("Description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath UpdateRoutingProfileName where
  toPath UpdateRoutingProfileName' {..} =
    Lude.mconcat
      [ "/routing-profiles/",
        Lude.toBS instanceId,
        "/",
        Lude.toBS routingProfileId,
        "/name"
      ]

instance Lude.ToQuery UpdateRoutingProfileName where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateRoutingProfileNameResponse' smart constructor.
data UpdateRoutingProfileNameResponse = UpdateRoutingProfileNameResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRoutingProfileNameResponse' with the minimum fields required to make a request.
mkUpdateRoutingProfileNameResponse ::
  UpdateRoutingProfileNameResponse
mkUpdateRoutingProfileNameResponse =
  UpdateRoutingProfileNameResponse'

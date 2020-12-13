{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates properties for an alias. To update properties, specify the alias ID to be updated and provide the information to be changed. To reassign an alias to another fleet, provide an updated routing strategy. If successful, the updated alias record is returned.
--
--
--     * 'CreateAlias'
--
--
--     * 'ListAliases'
--
--
--     * 'DescribeAlias'
--
--
--     * 'UpdateAlias'
--
--
--     * 'DeleteAlias'
--
--
--     * 'ResolveAlias'
module Network.AWS.GameLift.UpdateAlias
  ( -- * Creating a request
    UpdateAlias (..),
    mkUpdateAlias,

    -- ** Request lenses
    uaAliasId,
    uaRoutingStrategy,
    uaName,
    uaDescription,

    -- * Destructuring the response
    UpdateAliasResponse (..),
    mkUpdateAliasResponse,

    -- ** Response lenses
    uarsAlias,
    uarsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkUpdateAlias' smart constructor.
data UpdateAlias = UpdateAlias'
  { -- | A unique identifier for the alias that you want to update. You can use either the alias ID or ARN value.
    aliasId :: Lude.Text,
    -- | The routing configuration, including routing type and fleet target, for the alias.
    routingStrategy :: Lude.Maybe RoutingStrategy,
    -- | A descriptive label that is associated with an alias. Alias names do not need to be unique.
    name :: Lude.Maybe Lude.Text,
    -- | A human-readable description of the alias.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAlias' with the minimum fields required to make a request.
--
-- * 'aliasId' - A unique identifier for the alias that you want to update. You can use either the alias ID or ARN value.
-- * 'routingStrategy' - The routing configuration, including routing type and fleet target, for the alias.
-- * 'name' - A descriptive label that is associated with an alias. Alias names do not need to be unique.
-- * 'description' - A human-readable description of the alias.
mkUpdateAlias ::
  -- | 'aliasId'
  Lude.Text ->
  UpdateAlias
mkUpdateAlias pAliasId_ =
  UpdateAlias'
    { aliasId = pAliasId_,
      routingStrategy = Lude.Nothing,
      name = Lude.Nothing,
      description = Lude.Nothing
    }

-- | A unique identifier for the alias that you want to update. You can use either the alias ID or ARN value.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAliasId :: Lens.Lens' UpdateAlias Lude.Text
uaAliasId = Lens.lens (aliasId :: UpdateAlias -> Lude.Text) (\s a -> s {aliasId = a} :: UpdateAlias)
{-# DEPRECATED uaAliasId "Use generic-lens or generic-optics with 'aliasId' instead." #-}

-- | The routing configuration, including routing type and fleet target, for the alias.
--
-- /Note:/ Consider using 'routingStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaRoutingStrategy :: Lens.Lens' UpdateAlias (Lude.Maybe RoutingStrategy)
uaRoutingStrategy = Lens.lens (routingStrategy :: UpdateAlias -> Lude.Maybe RoutingStrategy) (\s a -> s {routingStrategy = a} :: UpdateAlias)
{-# DEPRECATED uaRoutingStrategy "Use generic-lens or generic-optics with 'routingStrategy' instead." #-}

-- | A descriptive label that is associated with an alias. Alias names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaName :: Lens.Lens' UpdateAlias (Lude.Maybe Lude.Text)
uaName = Lens.lens (name :: UpdateAlias -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateAlias)
{-# DEPRECATED uaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A human-readable description of the alias.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaDescription :: Lens.Lens' UpdateAlias (Lude.Maybe Lude.Text)
uaDescription = Lens.lens (description :: UpdateAlias -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateAlias)
{-# DEPRECATED uaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateAlias where
  type Rs UpdateAlias = UpdateAliasResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateAliasResponse'
            Lude.<$> (x Lude..?> "Alias") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateAlias where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.UpdateAlias" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateAlias where
  toJSON UpdateAlias' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AliasId" Lude..= aliasId),
            ("RoutingStrategy" Lude..=) Lude.<$> routingStrategy,
            ("Name" Lude..=) Lude.<$> name,
            ("Description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath UpdateAlias where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateAlias where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkUpdateAliasResponse' smart constructor.
data UpdateAliasResponse = UpdateAliasResponse'
  { -- | The updated alias resource.
    alias :: Lude.Maybe Alias,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAliasResponse' with the minimum fields required to make a request.
--
-- * 'alias' - The updated alias resource.
-- * 'responseStatus' - The response status code.
mkUpdateAliasResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateAliasResponse
mkUpdateAliasResponse pResponseStatus_ =
  UpdateAliasResponse'
    { alias = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The updated alias resource.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsAlias :: Lens.Lens' UpdateAliasResponse (Lude.Maybe Alias)
uarsAlias = Lens.lens (alias :: UpdateAliasResponse -> Lude.Maybe Alias) (\s a -> s {alias = a} :: UpdateAliasResponse)
{-# DEPRECATED uarsAlias "Use generic-lens or generic-optics with 'alias' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsResponseStatus :: Lens.Lens' UpdateAliasResponse Lude.Int
uarsResponseStatus = Lens.lens (responseStatus :: UpdateAliasResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateAliasResponse)
{-# DEPRECATED uarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

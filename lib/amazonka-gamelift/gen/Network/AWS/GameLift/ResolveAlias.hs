{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.ResolveAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the fleet ID that an alias is currently pointing to.
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
module Network.AWS.GameLift.ResolveAlias
  ( -- * Creating a request
    ResolveAlias (..),
    mkResolveAlias,

    -- ** Request lenses
    raAliasId,

    -- * Destructuring the response
    ResolveAliasResponse (..),
    mkResolveAliasResponse,

    -- ** Response lenses
    rarsFleetARN,
    rarsFleetId,
    rarsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkResolveAlias' smart constructor.
newtype ResolveAlias = ResolveAlias'
  { -- | The unique identifier of the alias that you want to retrieve a fleet ID for. You can use either the alias ID or ARN value.
    aliasId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResolveAlias' with the minimum fields required to make a request.
--
-- * 'aliasId' - The unique identifier of the alias that you want to retrieve a fleet ID for. You can use either the alias ID or ARN value.
mkResolveAlias ::
  -- | 'aliasId'
  Lude.Text ->
  ResolveAlias
mkResolveAlias pAliasId_ = ResolveAlias' {aliasId = pAliasId_}

-- | The unique identifier of the alias that you want to retrieve a fleet ID for. You can use either the alias ID or ARN value.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raAliasId :: Lens.Lens' ResolveAlias Lude.Text
raAliasId = Lens.lens (aliasId :: ResolveAlias -> Lude.Text) (\s a -> s {aliasId = a} :: ResolveAlias)
{-# DEPRECATED raAliasId "Use generic-lens or generic-optics with 'aliasId' instead." #-}

instance Lude.AWSRequest ResolveAlias where
  type Rs ResolveAlias = ResolveAliasResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          ResolveAliasResponse'
            Lude.<$> (x Lude..?> "FleetArn")
            Lude.<*> (x Lude..?> "FleetId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ResolveAlias where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.ResolveAlias" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ResolveAlias where
  toJSON ResolveAlias' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("AliasId" Lude..= aliasId)])

instance Lude.ToPath ResolveAlias where
  toPath = Lude.const "/"

instance Lude.ToQuery ResolveAlias where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkResolveAliasResponse' smart constructor.
data ResolveAliasResponse = ResolveAliasResponse'
  { -- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift fleet resource that this alias points to.
    fleetARN :: Lude.Maybe Lude.Text,
    -- | The fleet identifier that the alias is pointing to.
    fleetId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResolveAliasResponse' with the minimum fields required to make a request.
--
-- * 'fleetARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift fleet resource that this alias points to.
-- * 'fleetId' - The fleet identifier that the alias is pointing to.
-- * 'responseStatus' - The response status code.
mkResolveAliasResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ResolveAliasResponse
mkResolveAliasResponse pResponseStatus_ =
  ResolveAliasResponse'
    { fleetARN = Lude.Nothing,
      fleetId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift fleet resource that this alias points to.
--
-- /Note:/ Consider using 'fleetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rarsFleetARN :: Lens.Lens' ResolveAliasResponse (Lude.Maybe Lude.Text)
rarsFleetARN = Lens.lens (fleetARN :: ResolveAliasResponse -> Lude.Maybe Lude.Text) (\s a -> s {fleetARN = a} :: ResolveAliasResponse)
{-# DEPRECATED rarsFleetARN "Use generic-lens or generic-optics with 'fleetARN' instead." #-}

-- | The fleet identifier that the alias is pointing to.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rarsFleetId :: Lens.Lens' ResolveAliasResponse (Lude.Maybe Lude.Text)
rarsFleetId = Lens.lens (fleetId :: ResolveAliasResponse -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: ResolveAliasResponse)
{-# DEPRECATED rarsFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rarsResponseStatus :: Lens.Lens' ResolveAliasResponse Lude.Int
rarsResponseStatus = Lens.lens (responseStatus :: ResolveAliasResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ResolveAliasResponse)
{-# DEPRECATED rarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

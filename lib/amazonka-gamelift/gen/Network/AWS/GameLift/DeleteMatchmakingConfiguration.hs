{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeleteMatchmakingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently removes a FlexMatch matchmaking configuration. To delete, specify the configuration name. A matchmaking configuration cannot be deleted if it is being used in any active matchmaking tickets.
--
-- __Related operations__
--
--     * 'CreateMatchmakingConfiguration'
--
--
--     * 'DescribeMatchmakingConfigurations'
--
--
--     * 'UpdateMatchmakingConfiguration'
--
--
--     * 'DeleteMatchmakingConfiguration'
--
--
--     * 'CreateMatchmakingRuleSet'
--
--
--     * 'DescribeMatchmakingRuleSets'
--
--
--     * 'ValidateMatchmakingRuleSet'
--
--
--     * 'DeleteMatchmakingRuleSet'
module Network.AWS.GameLift.DeleteMatchmakingConfiguration
  ( -- * Creating a request
    DeleteMatchmakingConfiguration (..),
    mkDeleteMatchmakingConfiguration,

    -- ** Request lenses
    dmcName,

    -- * Destructuring the response
    DeleteMatchmakingConfigurationResponse (..),
    mkDeleteMatchmakingConfigurationResponse,

    -- ** Response lenses
    dmcrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDeleteMatchmakingConfiguration' smart constructor.
newtype DeleteMatchmakingConfiguration = DeleteMatchmakingConfiguration'
  { -- | A unique identifier for a matchmaking configuration. You can use either the configuration name or ARN value.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMatchmakingConfiguration' with the minimum fields required to make a request.
--
-- * 'name' - A unique identifier for a matchmaking configuration. You can use either the configuration name or ARN value.
mkDeleteMatchmakingConfiguration ::
  -- | 'name'
  Lude.Text ->
  DeleteMatchmakingConfiguration
mkDeleteMatchmakingConfiguration pName_ =
  DeleteMatchmakingConfiguration' {name = pName_}

-- | A unique identifier for a matchmaking configuration. You can use either the configuration name or ARN value.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcName :: Lens.Lens' DeleteMatchmakingConfiguration Lude.Text
dmcName = Lens.lens (name :: DeleteMatchmakingConfiguration -> Lude.Text) (\s a -> s {name = a} :: DeleteMatchmakingConfiguration)
{-# DEPRECATED dmcName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteMatchmakingConfiguration where
  type
    Rs DeleteMatchmakingConfiguration =
      DeleteMatchmakingConfigurationResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteMatchmakingConfigurationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteMatchmakingConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DeleteMatchmakingConfiguration" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteMatchmakingConfiguration where
  toJSON DeleteMatchmakingConfiguration' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DeleteMatchmakingConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteMatchmakingConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteMatchmakingConfigurationResponse' smart constructor.
newtype DeleteMatchmakingConfigurationResponse = DeleteMatchmakingConfigurationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMatchmakingConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteMatchmakingConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteMatchmakingConfigurationResponse
mkDeleteMatchmakingConfigurationResponse pResponseStatus_ =
  DeleteMatchmakingConfigurationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrsResponseStatus :: Lens.Lens' DeleteMatchmakingConfigurationResponse Lude.Int
dmcrsResponseStatus = Lens.lens (responseStatus :: DeleteMatchmakingConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteMatchmakingConfigurationResponse)
{-# DEPRECATED dmcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

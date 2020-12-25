{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.PutAccountSettingDefault
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an account setting for all IAM users on an account for whom no individual account setting has been specified. Account settings are set on a per-Region basis.
module Network.AWS.ECS.PutAccountSettingDefault
  ( -- * Creating a request
    PutAccountSettingDefault (..),
    mkPutAccountSettingDefault,

    -- ** Request lenses
    pasdName,
    pasdValue,

    -- * Destructuring the response
    PutAccountSettingDefaultResponse (..),
    mkPutAccountSettingDefaultResponse,

    -- ** Response lenses
    pasdrrsSetting,
    pasdrrsResponseStatus,
  )
where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutAccountSettingDefault' smart constructor.
data PutAccountSettingDefault = PutAccountSettingDefault'
  { -- | The resource name for which to modify the account setting. If @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS services is affected. If @taskLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS tasks is affected. If @containerInstanceLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS container instances is affected. If @awsvpcTrunking@ is specified, the ENI limit for your Amazon ECS container instances is affected. If @containerInsights@ is specified, the default setting for CloudWatch Container Insights for your clusters is affected.
    name :: Types.SettingName,
    -- | The account setting value for the specified principal ARN. Accepted values are @enabled@ and @disabled@ .
    value :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutAccountSettingDefault' value with any optional fields omitted.
mkPutAccountSettingDefault ::
  -- | 'name'
  Types.SettingName ->
  -- | 'value'
  Types.String ->
  PutAccountSettingDefault
mkPutAccountSettingDefault name value =
  PutAccountSettingDefault' {name, value}

-- | The resource name for which to modify the account setting. If @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS services is affected. If @taskLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS tasks is affected. If @containerInstanceLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS container instances is affected. If @awsvpcTrunking@ is specified, the ENI limit for your Amazon ECS container instances is affected. If @containerInsights@ is specified, the default setting for CloudWatch Container Insights for your clusters is affected.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasdName :: Lens.Lens' PutAccountSettingDefault Types.SettingName
pasdName = Lens.field @"name"
{-# DEPRECATED pasdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The account setting value for the specified principal ARN. Accepted values are @enabled@ and @disabled@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasdValue :: Lens.Lens' PutAccountSettingDefault Types.String
pasdValue = Lens.field @"value"
{-# DEPRECATED pasdValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON PutAccountSettingDefault where
  toJSON PutAccountSettingDefault {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("value" Core..= value)
          ]
      )

instance Core.AWSRequest PutAccountSettingDefault where
  type Rs PutAccountSettingDefault = PutAccountSettingDefaultResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerServiceV20141113.PutAccountSettingDefault"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutAccountSettingDefaultResponse'
            Core.<$> (x Core..:? "setting") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutAccountSettingDefaultResponse' smart constructor.
data PutAccountSettingDefaultResponse = PutAccountSettingDefaultResponse'
  { setting :: Core.Maybe Types.Setting,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutAccountSettingDefaultResponse' value with any optional fields omitted.
mkPutAccountSettingDefaultResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutAccountSettingDefaultResponse
mkPutAccountSettingDefaultResponse responseStatus =
  PutAccountSettingDefaultResponse'
    { setting = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'setting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasdrrsSetting :: Lens.Lens' PutAccountSettingDefaultResponse (Core.Maybe Types.Setting)
pasdrrsSetting = Lens.field @"setting"
{-# DEPRECATED pasdrrsSetting "Use generic-lens or generic-optics with 'setting' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasdrrsResponseStatus :: Lens.Lens' PutAccountSettingDefaultResponse Core.Int
pasdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pasdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about a parameter by using the parameter name. Don't confuse this API action with the 'GetParameters' API action.
module Network.AWS.SSM.GetParameter
  ( -- * Creating a request
    GetParameter (..),
    mkGetParameter,

    -- ** Request lenses
    gName,
    gWithDecryption,

    -- * Destructuring the response
    GetParameterResponse (..),
    mkGetParameterResponse,

    -- ** Response lenses
    gprfrsParameter,
    gprfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetParameter' smart constructor.
data GetParameter = GetParameter'
  { -- | The name of the parameter you want to query.
    name :: Types.PSParameterName,
    -- | Return decrypted values for secure string parameters. This flag is ignored for String and StringList parameter types.
    withDecryption :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetParameter' value with any optional fields omitted.
mkGetParameter ::
  -- | 'name'
  Types.PSParameterName ->
  GetParameter
mkGetParameter name =
  GetParameter' {name, withDecryption = Core.Nothing}

-- | The name of the parameter you want to query.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gName :: Lens.Lens' GetParameter Types.PSParameterName
gName = Lens.field @"name"
{-# DEPRECATED gName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Return decrypted values for secure string parameters. This flag is ignored for String and StringList parameter types.
--
-- /Note:/ Consider using 'withDecryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gWithDecryption :: Lens.Lens' GetParameter (Core.Maybe Core.Bool)
gWithDecryption = Lens.field @"withDecryption"
{-# DEPRECATED gWithDecryption "Use generic-lens or generic-optics with 'withDecryption' instead." #-}

instance Core.FromJSON GetParameter where
  toJSON GetParameter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("WithDecryption" Core..=) Core.<$> withDecryption
          ]
      )

instance Core.AWSRequest GetParameter where
  type Rs GetParameter = GetParameterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.GetParameter")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetParameterResponse'
            Core.<$> (x Core..:? "Parameter") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetParameterResponse' smart constructor.
data GetParameterResponse = GetParameterResponse'
  { -- | Information about a parameter.
    parameter :: Core.Maybe Types.Parameter,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetParameterResponse' value with any optional fields omitted.
mkGetParameterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetParameterResponse
mkGetParameterResponse responseStatus =
  GetParameterResponse' {parameter = Core.Nothing, responseStatus}

-- | Information about a parameter.
--
-- /Note:/ Consider using 'parameter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprfrsParameter :: Lens.Lens' GetParameterResponse (Core.Maybe Types.Parameter)
gprfrsParameter = Lens.field @"parameter"
{-# DEPRECATED gprfrsParameter "Use generic-lens or generic-optics with 'parameter' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprfrsResponseStatus :: Lens.Lens' GetParameterResponse Core.Int
gprfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gprfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

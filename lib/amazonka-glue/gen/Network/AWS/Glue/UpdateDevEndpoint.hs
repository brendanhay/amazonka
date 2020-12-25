{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateDevEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified development endpoint.
module Network.AWS.Glue.UpdateDevEndpoint
  ( -- * Creating a request
    UpdateDevEndpoint (..),
    mkUpdateDevEndpoint,

    -- ** Request lenses
    udeEndpointName,
    udeAddArguments,
    udeAddPublicKeys,
    udeCustomLibraries,
    udeDeleteArguments,
    udeDeletePublicKeys,
    udePublicKey,
    udeUpdateEtlLibraries,

    -- * Destructuring the response
    UpdateDevEndpointResponse (..),
    mkUpdateDevEndpointResponse,

    -- ** Response lenses
    uderrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateDevEndpoint' smart constructor.
data UpdateDevEndpoint = UpdateDevEndpoint'
  { -- | The name of the @DevEndpoint@ to be updated.
    endpointName :: Types.EndpointName,
    -- | The map of arguments to add the map of arguments used to configure the @DevEndpoint@ .
    --
    -- Valid arguments are:
    --
    --     * @"--enable-glue-datacatalog": ""@
    --
    --
    --     * @"GLUE_PYTHON_VERSION": "3"@
    --
    --
    --     * @"GLUE_PYTHON_VERSION": "2"@
    --
    --
    -- You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
    addArguments :: Core.Maybe (Core.HashMap Types.GenericString Types.GenericString),
    -- | The list of public keys for the @DevEndpoint@ to use.
    addPublicKeys :: Core.Maybe [Types.GenericString],
    -- | Custom Python or Java libraries to be loaded in the @DevEndpoint@ .
    customLibraries :: Core.Maybe Types.DevEndpointCustomLibraries,
    -- | The list of argument keys to be deleted from the map of arguments used to configure the @DevEndpoint@ .
    deleteArguments :: Core.Maybe [Types.GenericString],
    -- | The list of public keys to be deleted from the @DevEndpoint@ .
    deletePublicKeys :: Core.Maybe [Types.GenericString],
    -- | The public key for the @DevEndpoint@ to use.
    publicKey :: Core.Maybe Types.PublicKey,
    -- | @True@ if the list of custom libraries to be loaded in the development endpoint needs to be updated, or @False@ if otherwise.
    updateEtlLibraries :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDevEndpoint' value with any optional fields omitted.
mkUpdateDevEndpoint ::
  -- | 'endpointName'
  Types.EndpointName ->
  UpdateDevEndpoint
mkUpdateDevEndpoint endpointName =
  UpdateDevEndpoint'
    { endpointName,
      addArguments = Core.Nothing,
      addPublicKeys = Core.Nothing,
      customLibraries = Core.Nothing,
      deleteArguments = Core.Nothing,
      deletePublicKeys = Core.Nothing,
      publicKey = Core.Nothing,
      updateEtlLibraries = Core.Nothing
    }

-- | The name of the @DevEndpoint@ to be updated.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeEndpointName :: Lens.Lens' UpdateDevEndpoint Types.EndpointName
udeEndpointName = Lens.field @"endpointName"
{-# DEPRECATED udeEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | The map of arguments to add the map of arguments used to configure the @DevEndpoint@ .
--
-- Valid arguments are:
--
--     * @"--enable-glue-datacatalog": ""@
--
--
--     * @"GLUE_PYTHON_VERSION": "3"@
--
--
--     * @"GLUE_PYTHON_VERSION": "2"@
--
--
-- You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
--
-- /Note:/ Consider using 'addArguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeAddArguments :: Lens.Lens' UpdateDevEndpoint (Core.Maybe (Core.HashMap Types.GenericString Types.GenericString))
udeAddArguments = Lens.field @"addArguments"
{-# DEPRECATED udeAddArguments "Use generic-lens or generic-optics with 'addArguments' instead." #-}

-- | The list of public keys for the @DevEndpoint@ to use.
--
-- /Note:/ Consider using 'addPublicKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeAddPublicKeys :: Lens.Lens' UpdateDevEndpoint (Core.Maybe [Types.GenericString])
udeAddPublicKeys = Lens.field @"addPublicKeys"
{-# DEPRECATED udeAddPublicKeys "Use generic-lens or generic-optics with 'addPublicKeys' instead." #-}

-- | Custom Python or Java libraries to be loaded in the @DevEndpoint@ .
--
-- /Note:/ Consider using 'customLibraries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeCustomLibraries :: Lens.Lens' UpdateDevEndpoint (Core.Maybe Types.DevEndpointCustomLibraries)
udeCustomLibraries = Lens.field @"customLibraries"
{-# DEPRECATED udeCustomLibraries "Use generic-lens or generic-optics with 'customLibraries' instead." #-}

-- | The list of argument keys to be deleted from the map of arguments used to configure the @DevEndpoint@ .
--
-- /Note:/ Consider using 'deleteArguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeDeleteArguments :: Lens.Lens' UpdateDevEndpoint (Core.Maybe [Types.GenericString])
udeDeleteArguments = Lens.field @"deleteArguments"
{-# DEPRECATED udeDeleteArguments "Use generic-lens or generic-optics with 'deleteArguments' instead." #-}

-- | The list of public keys to be deleted from the @DevEndpoint@ .
--
-- /Note:/ Consider using 'deletePublicKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeDeletePublicKeys :: Lens.Lens' UpdateDevEndpoint (Core.Maybe [Types.GenericString])
udeDeletePublicKeys = Lens.field @"deletePublicKeys"
{-# DEPRECATED udeDeletePublicKeys "Use generic-lens or generic-optics with 'deletePublicKeys' instead." #-}

-- | The public key for the @DevEndpoint@ to use.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udePublicKey :: Lens.Lens' UpdateDevEndpoint (Core.Maybe Types.PublicKey)
udePublicKey = Lens.field @"publicKey"
{-# DEPRECATED udePublicKey "Use generic-lens or generic-optics with 'publicKey' instead." #-}

-- | @True@ if the list of custom libraries to be loaded in the development endpoint needs to be updated, or @False@ if otherwise.
--
-- /Note:/ Consider using 'updateEtlLibraries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeUpdateEtlLibraries :: Lens.Lens' UpdateDevEndpoint (Core.Maybe Core.Bool)
udeUpdateEtlLibraries = Lens.field @"updateEtlLibraries"
{-# DEPRECATED udeUpdateEtlLibraries "Use generic-lens or generic-optics with 'updateEtlLibraries' instead." #-}

instance Core.FromJSON UpdateDevEndpoint where
  toJSON UpdateDevEndpoint {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EndpointName" Core..= endpointName),
            ("AddArguments" Core..=) Core.<$> addArguments,
            ("AddPublicKeys" Core..=) Core.<$> addPublicKeys,
            ("CustomLibraries" Core..=) Core.<$> customLibraries,
            ("DeleteArguments" Core..=) Core.<$> deleteArguments,
            ("DeletePublicKeys" Core..=) Core.<$> deletePublicKeys,
            ("PublicKey" Core..=) Core.<$> publicKey,
            ("UpdateEtlLibraries" Core..=) Core.<$> updateEtlLibraries
          ]
      )

instance Core.AWSRequest UpdateDevEndpoint where
  type Rs UpdateDevEndpoint = UpdateDevEndpointResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.UpdateDevEndpoint")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDevEndpointResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateDevEndpointResponse' smart constructor.
newtype UpdateDevEndpointResponse = UpdateDevEndpointResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDevEndpointResponse' value with any optional fields omitted.
mkUpdateDevEndpointResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateDevEndpointResponse
mkUpdateDevEndpointResponse responseStatus =
  UpdateDevEndpointResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uderrsResponseStatus :: Lens.Lens' UpdateDevEndpointResponse Core.Int
uderrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uderrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

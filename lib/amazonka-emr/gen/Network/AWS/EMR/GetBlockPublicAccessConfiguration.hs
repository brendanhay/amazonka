{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.GetBlockPublicAccessConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the Amazon EMR block public access configuration for your AWS account in the current Region. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/configure-block-public-access.html Configure Block Public Access for Amazon EMR> in the /Amazon EMR Management Guide/ .
module Network.AWS.EMR.GetBlockPublicAccessConfiguration
  ( -- * Creating a request
    GetBlockPublicAccessConfiguration (..),
    mkGetBlockPublicAccessConfiguration,

    -- * Destructuring the response
    GetBlockPublicAccessConfigurationResponse (..),
    mkGetBlockPublicAccessConfigurationResponse,

    -- ** Response lenses
    gbpacrrsBlockPublicAccessConfiguration,
    gbpacrrsBlockPublicAccessConfigurationMetadata,
    gbpacrrsResponseStatus,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetBlockPublicAccessConfiguration' smart constructor.
data GetBlockPublicAccessConfiguration = GetBlockPublicAccessConfiguration'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBlockPublicAccessConfiguration' value with any optional fields omitted.
mkGetBlockPublicAccessConfiguration ::
  GetBlockPublicAccessConfiguration
mkGetBlockPublicAccessConfiguration =
  GetBlockPublicAccessConfiguration'

instance Core.FromJSON GetBlockPublicAccessConfiguration where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetBlockPublicAccessConfiguration where
  type
    Rs GetBlockPublicAccessConfiguration =
      GetBlockPublicAccessConfigurationResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "ElasticMapReduce.GetBlockPublicAccessConfiguration"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBlockPublicAccessConfigurationResponse'
            Core.<$> (x Core..: "BlockPublicAccessConfiguration")
            Core.<*> (x Core..: "BlockPublicAccessConfigurationMetadata")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetBlockPublicAccessConfigurationResponse' smart constructor.
data GetBlockPublicAccessConfigurationResponse = GetBlockPublicAccessConfigurationResponse'
  { -- | A configuration for Amazon EMR block public access. The configuration applies to all clusters created in your account for the current Region. The configuration specifies whether block public access is enabled. If block public access is enabled, security groups associated with the cluster cannot have rules that allow inbound traffic from 0.0.0.0/0 or ::/0 on a port, unless the port is specified as an exception using @PermittedPublicSecurityGroupRuleRanges@ in the @BlockPublicAccessConfiguration@ . By default, Port 22 (SSH) is an exception, and public access is allowed on this port. You can change this by updating the block public access configuration to remove the exception.
    blockPublicAccessConfiguration :: Types.BlockPublicAccessConfiguration,
    -- | Properties that describe the AWS principal that created the @BlockPublicAccessConfiguration@ using the @PutBlockPublicAccessConfiguration@ action as well as the date and time that the configuration was created. Each time a configuration for block public access is updated, Amazon EMR updates this metadata.
    blockPublicAccessConfigurationMetadata :: Types.BlockPublicAccessConfigurationMetadata,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetBlockPublicAccessConfigurationResponse' value with any optional fields omitted.
mkGetBlockPublicAccessConfigurationResponse ::
  -- | 'blockPublicAccessConfiguration'
  Types.BlockPublicAccessConfiguration ->
  -- | 'blockPublicAccessConfigurationMetadata'
  Types.BlockPublicAccessConfigurationMetadata ->
  -- | 'responseStatus'
  Core.Int ->
  GetBlockPublicAccessConfigurationResponse
mkGetBlockPublicAccessConfigurationResponse
  blockPublicAccessConfiguration
  blockPublicAccessConfigurationMetadata
  responseStatus =
    GetBlockPublicAccessConfigurationResponse'
      { blockPublicAccessConfiguration,
        blockPublicAccessConfigurationMetadata,
        responseStatus
      }

-- | A configuration for Amazon EMR block public access. The configuration applies to all clusters created in your account for the current Region. The configuration specifies whether block public access is enabled. If block public access is enabled, security groups associated with the cluster cannot have rules that allow inbound traffic from 0.0.0.0/0 or ::/0 on a port, unless the port is specified as an exception using @PermittedPublicSecurityGroupRuleRanges@ in the @BlockPublicAccessConfiguration@ . By default, Port 22 (SSH) is an exception, and public access is allowed on this port. You can change this by updating the block public access configuration to remove the exception.
--
-- /Note:/ Consider using 'blockPublicAccessConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpacrrsBlockPublicAccessConfiguration :: Lens.Lens' GetBlockPublicAccessConfigurationResponse Types.BlockPublicAccessConfiguration
gbpacrrsBlockPublicAccessConfiguration = Lens.field @"blockPublicAccessConfiguration"
{-# DEPRECATED gbpacrrsBlockPublicAccessConfiguration "Use generic-lens or generic-optics with 'blockPublicAccessConfiguration' instead." #-}

-- | Properties that describe the AWS principal that created the @BlockPublicAccessConfiguration@ using the @PutBlockPublicAccessConfiguration@ action as well as the date and time that the configuration was created. Each time a configuration for block public access is updated, Amazon EMR updates this metadata.
--
-- /Note:/ Consider using 'blockPublicAccessConfigurationMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpacrrsBlockPublicAccessConfigurationMetadata :: Lens.Lens' GetBlockPublicAccessConfigurationResponse Types.BlockPublicAccessConfigurationMetadata
gbpacrrsBlockPublicAccessConfigurationMetadata = Lens.field @"blockPublicAccessConfigurationMetadata"
{-# DEPRECATED gbpacrrsBlockPublicAccessConfigurationMetadata "Use generic-lens or generic-optics with 'blockPublicAccessConfigurationMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpacrrsResponseStatus :: Lens.Lens' GetBlockPublicAccessConfigurationResponse Core.Int
gbpacrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gbpacrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

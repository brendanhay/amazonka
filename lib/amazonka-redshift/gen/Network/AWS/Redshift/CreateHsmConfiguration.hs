{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateHsmConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an HSM configuration that contains the information required by an Amazon Redshift cluster to store and use database encryption keys in a Hardware Security Module (HSM). After creating the HSM configuration, you can specify it as a parameter when creating a cluster. The cluster will then store its encryption keys in the HSM.
--
-- In addition to creating an HSM configuration, you must also create an HSM client certificate. For more information, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-HSM.html Hardware Security Modules> in the Amazon Redshift Cluster Management Guide.
module Network.AWS.Redshift.CreateHsmConfiguration
    (
    -- * Creating a request
      CreateHsmConfiguration (..)
    , mkCreateHsmConfiguration
    -- ** Request lenses
    , chcHsmConfigurationIdentifier
    , chcDescription
    , chcHsmIpAddress
    , chcHsmPartitionName
    , chcHsmPartitionPassword
    , chcHsmServerPublicCertificate
    , chcTags

    -- * Destructuring the response
    , CreateHsmConfigurationResponse (..)
    , mkCreateHsmConfigurationResponse
    -- ** Response lenses
    , chcrrsHsmConfiguration
    , chcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkCreateHsmConfiguration' smart constructor.
data CreateHsmConfiguration = CreateHsmConfiguration'
  { hsmConfigurationIdentifier :: Core.Text
    -- ^ The identifier to be assigned to the new Amazon Redshift HSM configuration.
  , description :: Core.Text
    -- ^ A text description of the HSM configuration to be created.
  , hsmIpAddress :: Core.Text
    -- ^ The IP address that the Amazon Redshift cluster must use to access the HSM.
  , hsmPartitionName :: Core.Text
    -- ^ The name of the partition in the HSM where the Amazon Redshift clusters will store their database encryption keys.
  , hsmPartitionPassword :: Core.Text
    -- ^ The password required to access the HSM partition.
  , hsmServerPublicCertificate :: Core.Text
    -- ^ The HSMs public certificate file. When using Cloud HSM, the file name is server.pem.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of tag instances.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateHsmConfiguration' value with any optional fields omitted.
mkCreateHsmConfiguration
    :: Core.Text -- ^ 'hsmConfigurationIdentifier'
    -> Core.Text -- ^ 'description'
    -> Core.Text -- ^ 'hsmIpAddress'
    -> Core.Text -- ^ 'hsmPartitionName'
    -> Core.Text -- ^ 'hsmPartitionPassword'
    -> Core.Text -- ^ 'hsmServerPublicCertificate'
    -> CreateHsmConfiguration
mkCreateHsmConfiguration hsmConfigurationIdentifier description
  hsmIpAddress hsmPartitionName hsmPartitionPassword
  hsmServerPublicCertificate
  = CreateHsmConfiguration'{hsmConfigurationIdentifier, description,
                            hsmIpAddress, hsmPartitionName, hsmPartitionPassword,
                            hsmServerPublicCertificate, tags = Core.Nothing}

-- | The identifier to be assigned to the new Amazon Redshift HSM configuration.
--
-- /Note:/ Consider using 'hsmConfigurationIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcHsmConfigurationIdentifier :: Lens.Lens' CreateHsmConfiguration Core.Text
chcHsmConfigurationIdentifier = Lens.field @"hsmConfigurationIdentifier"
{-# INLINEABLE chcHsmConfigurationIdentifier #-}
{-# DEPRECATED hsmConfigurationIdentifier "Use generic-lens or generic-optics with 'hsmConfigurationIdentifier' instead"  #-}

-- | A text description of the HSM configuration to be created.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcDescription :: Lens.Lens' CreateHsmConfiguration Core.Text
chcDescription = Lens.field @"description"
{-# INLINEABLE chcDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The IP address that the Amazon Redshift cluster must use to access the HSM.
--
-- /Note:/ Consider using 'hsmIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcHsmIpAddress :: Lens.Lens' CreateHsmConfiguration Core.Text
chcHsmIpAddress = Lens.field @"hsmIpAddress"
{-# INLINEABLE chcHsmIpAddress #-}
{-# DEPRECATED hsmIpAddress "Use generic-lens or generic-optics with 'hsmIpAddress' instead"  #-}

-- | The name of the partition in the HSM where the Amazon Redshift clusters will store their database encryption keys.
--
-- /Note:/ Consider using 'hsmPartitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcHsmPartitionName :: Lens.Lens' CreateHsmConfiguration Core.Text
chcHsmPartitionName = Lens.field @"hsmPartitionName"
{-# INLINEABLE chcHsmPartitionName #-}
{-# DEPRECATED hsmPartitionName "Use generic-lens or generic-optics with 'hsmPartitionName' instead"  #-}

-- | The password required to access the HSM partition.
--
-- /Note:/ Consider using 'hsmPartitionPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcHsmPartitionPassword :: Lens.Lens' CreateHsmConfiguration Core.Text
chcHsmPartitionPassword = Lens.field @"hsmPartitionPassword"
{-# INLINEABLE chcHsmPartitionPassword #-}
{-# DEPRECATED hsmPartitionPassword "Use generic-lens or generic-optics with 'hsmPartitionPassword' instead"  #-}

-- | The HSMs public certificate file. When using Cloud HSM, the file name is server.pem.
--
-- /Note:/ Consider using 'hsmServerPublicCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcHsmServerPublicCertificate :: Lens.Lens' CreateHsmConfiguration Core.Text
chcHsmServerPublicCertificate = Lens.field @"hsmServerPublicCertificate"
{-# INLINEABLE chcHsmServerPublicCertificate #-}
{-# DEPRECATED hsmServerPublicCertificate "Use generic-lens or generic-optics with 'hsmServerPublicCertificate' instead"  #-}

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcTags :: Lens.Lens' CreateHsmConfiguration (Core.Maybe [Types.Tag])
chcTags = Lens.field @"tags"
{-# INLINEABLE chcTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateHsmConfiguration where
        toQuery CreateHsmConfiguration{..}
          = Core.toQueryPair "Action" ("CreateHsmConfiguration" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "HsmConfigurationIdentifier"
                hsmConfigurationIdentifier
              Core.<> Core.toQueryPair "Description" description
              Core.<> Core.toQueryPair "HsmIpAddress" hsmIpAddress
              Core.<> Core.toQueryPair "HsmPartitionName" hsmPartitionName
              Core.<>
              Core.toQueryPair "HsmPartitionPassword" hsmPartitionPassword
              Core.<>
              Core.toQueryPair "HsmServerPublicCertificate"
                hsmServerPublicCertificate
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)

instance Core.ToHeaders CreateHsmConfiguration where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateHsmConfiguration where
        type Rs CreateHsmConfiguration = CreateHsmConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "CreateHsmConfigurationResult"
              (\ s h x ->
                 CreateHsmConfigurationResponse' Core.<$>
                   (x Core..@? "HsmConfiguration") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateHsmConfigurationResponse' smart constructor.
data CreateHsmConfigurationResponse = CreateHsmConfigurationResponse'
  { hsmConfiguration :: Core.Maybe Types.HsmConfiguration
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateHsmConfigurationResponse' value with any optional fields omitted.
mkCreateHsmConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateHsmConfigurationResponse
mkCreateHsmConfigurationResponse responseStatus
  = CreateHsmConfigurationResponse'{hsmConfiguration = Core.Nothing,
                                    responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hsmConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcrrsHsmConfiguration :: Lens.Lens' CreateHsmConfigurationResponse (Core.Maybe Types.HsmConfiguration)
chcrrsHsmConfiguration = Lens.field @"hsmConfiguration"
{-# INLINEABLE chcrrsHsmConfiguration #-}
{-# DEPRECATED hsmConfiguration "Use generic-lens or generic-optics with 'hsmConfiguration' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcrrsResponseStatus :: Lens.Lens' CreateHsmConfigurationResponse Core.Int
chcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE chcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

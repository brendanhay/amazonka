{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.Namespace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.Namespace
  ( Namespace (..)
  -- * Smart constructor
  , mkNamespace
  -- * Lenses
  , nArn
  , nCreateDate
  , nCreatorRequestId
  , nDescription
  , nId
  , nName
  , nProperties
  , nServiceCount
  , nType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53AutoNaming.Types.Arn as Types
import qualified Network.AWS.Route53AutoNaming.Types.Description as Types
import qualified Network.AWS.Route53AutoNaming.Types.Name as Types
import qualified Network.AWS.Route53AutoNaming.Types.NamespaceProperties as Types
import qualified Network.AWS.Route53AutoNaming.Types.NamespaceType as Types
import qualified Network.AWS.Route53AutoNaming.Types.ResourceId as Types

-- | A complex type that contains information about a specified namespace.
--
-- /See:/ 'mkNamespace' smart constructor.
data Namespace = Namespace'
  { arn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the namespace when you create it.
  , createDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the namespace was created, in Unix date/time format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
  , creatorRequestId :: Core.Maybe Types.ResourceId
    -- ^ A unique string that identifies the request and that allows failed requests to be retried without the risk of executing an operation twice. 
  , description :: Core.Maybe Types.Description
    -- ^ The description that you specify for the namespace when you create it.
  , id :: Core.Maybe Types.ResourceId
    -- ^ The ID of a namespace.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the namespace, such as @example.com@ .
  , properties :: Core.Maybe Types.NamespaceProperties
    -- ^ A complex type that contains information that's specific to the type of the namespace.
  , serviceCount :: Core.Maybe Core.Int
    -- ^ The number of services that are associated with the namespace.
  , type' :: Core.Maybe Types.NamespaceType
    -- ^ The type of the namespace. The methods for discovering instances depends on the value that you specify:
--
--
--     * @HTTP@ : Instances can be discovered only programmatically, using the AWS Cloud Map @DiscoverInstances@ API.
--
--
--     * @DNS_PUBLIC@ : Instances can be discovered using public DNS queries and using the @DiscoverInstances@ API.
--
--
--     * @DNS_PRIVATE@ : Instances can be discovered using DNS queries in VPCs and using the @DiscoverInstances@ API.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Namespace' value with any optional fields omitted.
mkNamespace
    :: Namespace
mkNamespace
  = Namespace'{arn = Core.Nothing, createDate = Core.Nothing,
               creatorRequestId = Core.Nothing, description = Core.Nothing,
               id = Core.Nothing, name = Core.Nothing, properties = Core.Nothing,
               serviceCount = Core.Nothing, type' = Core.Nothing}

-- | The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the namespace when you create it.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nArn :: Lens.Lens' Namespace (Core.Maybe Types.Arn)
nArn = Lens.field @"arn"
{-# INLINEABLE nArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The date that the namespace was created, in Unix date/time format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nCreateDate :: Lens.Lens' Namespace (Core.Maybe Core.NominalDiffTime)
nCreateDate = Lens.field @"createDate"
{-# INLINEABLE nCreateDate #-}
{-# DEPRECATED createDate "Use generic-lens or generic-optics with 'createDate' instead"  #-}

-- | A unique string that identifies the request and that allows failed requests to be retried without the risk of executing an operation twice. 
--
-- /Note:/ Consider using 'creatorRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nCreatorRequestId :: Lens.Lens' Namespace (Core.Maybe Types.ResourceId)
nCreatorRequestId = Lens.field @"creatorRequestId"
{-# INLINEABLE nCreatorRequestId #-}
{-# DEPRECATED creatorRequestId "Use generic-lens or generic-optics with 'creatorRequestId' instead"  #-}

-- | The description that you specify for the namespace when you create it.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nDescription :: Lens.Lens' Namespace (Core.Maybe Types.Description)
nDescription = Lens.field @"description"
{-# INLINEABLE nDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The ID of a namespace.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nId :: Lens.Lens' Namespace (Core.Maybe Types.ResourceId)
nId = Lens.field @"id"
{-# INLINEABLE nId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the namespace, such as @example.com@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nName :: Lens.Lens' Namespace (Core.Maybe Types.Name)
nName = Lens.field @"name"
{-# INLINEABLE nName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A complex type that contains information that's specific to the type of the namespace.
--
-- /Note:/ Consider using 'properties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nProperties :: Lens.Lens' Namespace (Core.Maybe Types.NamespaceProperties)
nProperties = Lens.field @"properties"
{-# INLINEABLE nProperties #-}
{-# DEPRECATED properties "Use generic-lens or generic-optics with 'properties' instead"  #-}

-- | The number of services that are associated with the namespace.
--
-- /Note:/ Consider using 'serviceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nServiceCount :: Lens.Lens' Namespace (Core.Maybe Core.Int)
nServiceCount = Lens.field @"serviceCount"
{-# INLINEABLE nServiceCount #-}
{-# DEPRECATED serviceCount "Use generic-lens or generic-optics with 'serviceCount' instead"  #-}

-- | The type of the namespace. The methods for discovering instances depends on the value that you specify:
--
--
--     * @HTTP@ : Instances can be discovered only programmatically, using the AWS Cloud Map @DiscoverInstances@ API.
--
--
--     * @DNS_PUBLIC@ : Instances can be discovered using public DNS queries and using the @DiscoverInstances@ API.
--
--
--     * @DNS_PRIVATE@ : Instances can be discovered using DNS queries in VPCs and using the @DiscoverInstances@ API.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nType :: Lens.Lens' Namespace (Core.Maybe Types.NamespaceType)
nType = Lens.field @"type'"
{-# INLINEABLE nType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON Namespace where
        parseJSON
          = Core.withObject "Namespace" Core.$
              \ x ->
                Namespace' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "CreateDate" Core.<*>
                    x Core..:? "CreatorRequestId"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Properties"
                    Core.<*> x Core..:? "ServiceCount"
                    Core.<*> x Core..:? "Type"

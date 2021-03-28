{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.RelationalDatabaseSnapshot
  ( RelationalDatabaseSnapshot (..)
  -- * Smart constructor
  , mkRelationalDatabaseSnapshot
  -- * Lenses
  , rdsArn
  , rdsCreatedAt
  , rdsEngine
  , rdsEngineVersion
  , rdsFromRelationalDatabaseArn
  , rdsFromRelationalDatabaseBlueprintId
  , rdsFromRelationalDatabaseBundleId
  , rdsFromRelationalDatabaseName
  , rdsLocation
  , rdsName
  , rdsResourceType
  , rdsSizeInGb
  , rdsState
  , rdsSupportCode
  , rdsTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.Arn as Types
import qualified Network.AWS.Lightsail.Types.Engine as Types
import qualified Network.AWS.Lightsail.Types.EngineVersion as Types
import qualified Network.AWS.Lightsail.Types.FromRelationalDatabaseArn as Types
import qualified Network.AWS.Lightsail.Types.FromRelationalDatabaseName as Types
import qualified Network.AWS.Lightsail.Types.Name as Types
import qualified Network.AWS.Lightsail.Types.ResourceLocation as Types
import qualified Network.AWS.Lightsail.Types.ResourceType as Types
import qualified Network.AWS.Lightsail.Types.State as Types
import qualified Network.AWS.Lightsail.Types.Tag as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a database snapshot.
--
-- /See:/ 'mkRelationalDatabaseSnapshot' smart constructor.
data RelationalDatabaseSnapshot = RelationalDatabaseSnapshot'
  { arn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the database snapshot.
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the database snapshot was created.
  , engine :: Core.Maybe Types.Engine
    -- ^ The software of the database snapshot (for example, @MySQL@ )
  , engineVersion :: Core.Maybe Types.EngineVersion
    -- ^ The database engine version for the database snapshot (for example, @5.7.23@ ).
  , fromRelationalDatabaseArn :: Core.Maybe Types.FromRelationalDatabaseArn
    -- ^ The Amazon Resource Name (ARN) of the database from which the database snapshot was created.
  , fromRelationalDatabaseBlueprintId :: Core.Maybe Core.Text
    -- ^ The blueprint ID of the database from which the database snapshot was created. A blueprint describes the major engine version of a database.
  , fromRelationalDatabaseBundleId :: Core.Maybe Core.Text
    -- ^ The bundle ID of the database from which the database snapshot was created.
  , fromRelationalDatabaseName :: Core.Maybe Types.FromRelationalDatabaseName
    -- ^ The name of the source database from which the database snapshot was created.
  , location :: Core.Maybe Types.ResourceLocation
    -- ^ The Region name and Availability Zone where the database snapshot is located.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the database snapshot.
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The Lightsail resource type.
  , sizeInGb :: Core.Maybe Core.Int
    -- ^ The size of the disk in GB (for example, @32@ ) for the database snapshot.
  , state :: Core.Maybe Types.State
    -- ^ The state of the database snapshot.
  , supportCode :: Core.Maybe Core.Text
    -- ^ The support code for the database snapshot. Include this code in your email to support when you have questions about a database snapshot in Lightsail. This code enables our support team to look up your Lightsail information more easily.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RelationalDatabaseSnapshot' value with any optional fields omitted.
mkRelationalDatabaseSnapshot
    :: RelationalDatabaseSnapshot
mkRelationalDatabaseSnapshot
  = RelationalDatabaseSnapshot'{arn = Core.Nothing,
                                createdAt = Core.Nothing, engine = Core.Nothing,
                                engineVersion = Core.Nothing,
                                fromRelationalDatabaseArn = Core.Nothing,
                                fromRelationalDatabaseBlueprintId = Core.Nothing,
                                fromRelationalDatabaseBundleId = Core.Nothing,
                                fromRelationalDatabaseName = Core.Nothing, location = Core.Nothing,
                                name = Core.Nothing, resourceType = Core.Nothing,
                                sizeInGb = Core.Nothing, state = Core.Nothing,
                                supportCode = Core.Nothing, tags = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the database snapshot.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsArn :: Lens.Lens' RelationalDatabaseSnapshot (Core.Maybe Types.Arn)
rdsArn = Lens.field @"arn"
{-# INLINEABLE rdsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The timestamp when the database snapshot was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsCreatedAt :: Lens.Lens' RelationalDatabaseSnapshot (Core.Maybe Core.NominalDiffTime)
rdsCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE rdsCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The software of the database snapshot (for example, @MySQL@ )
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsEngine :: Lens.Lens' RelationalDatabaseSnapshot (Core.Maybe Types.Engine)
rdsEngine = Lens.field @"engine"
{-# INLINEABLE rdsEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | The database engine version for the database snapshot (for example, @5.7.23@ ).
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsEngineVersion :: Lens.Lens' RelationalDatabaseSnapshot (Core.Maybe Types.EngineVersion)
rdsEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE rdsEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | The Amazon Resource Name (ARN) of the database from which the database snapshot was created.
--
-- /Note:/ Consider using 'fromRelationalDatabaseArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsFromRelationalDatabaseArn :: Lens.Lens' RelationalDatabaseSnapshot (Core.Maybe Types.FromRelationalDatabaseArn)
rdsFromRelationalDatabaseArn = Lens.field @"fromRelationalDatabaseArn"
{-# INLINEABLE rdsFromRelationalDatabaseArn #-}
{-# DEPRECATED fromRelationalDatabaseArn "Use generic-lens or generic-optics with 'fromRelationalDatabaseArn' instead"  #-}

-- | The blueprint ID of the database from which the database snapshot was created. A blueprint describes the major engine version of a database.
--
-- /Note:/ Consider using 'fromRelationalDatabaseBlueprintId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsFromRelationalDatabaseBlueprintId :: Lens.Lens' RelationalDatabaseSnapshot (Core.Maybe Core.Text)
rdsFromRelationalDatabaseBlueprintId = Lens.field @"fromRelationalDatabaseBlueprintId"
{-# INLINEABLE rdsFromRelationalDatabaseBlueprintId #-}
{-# DEPRECATED fromRelationalDatabaseBlueprintId "Use generic-lens or generic-optics with 'fromRelationalDatabaseBlueprintId' instead"  #-}

-- | The bundle ID of the database from which the database snapshot was created.
--
-- /Note:/ Consider using 'fromRelationalDatabaseBundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsFromRelationalDatabaseBundleId :: Lens.Lens' RelationalDatabaseSnapshot (Core.Maybe Core.Text)
rdsFromRelationalDatabaseBundleId = Lens.field @"fromRelationalDatabaseBundleId"
{-# INLINEABLE rdsFromRelationalDatabaseBundleId #-}
{-# DEPRECATED fromRelationalDatabaseBundleId "Use generic-lens or generic-optics with 'fromRelationalDatabaseBundleId' instead"  #-}

-- | The name of the source database from which the database snapshot was created.
--
-- /Note:/ Consider using 'fromRelationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsFromRelationalDatabaseName :: Lens.Lens' RelationalDatabaseSnapshot (Core.Maybe Types.FromRelationalDatabaseName)
rdsFromRelationalDatabaseName = Lens.field @"fromRelationalDatabaseName"
{-# INLINEABLE rdsFromRelationalDatabaseName #-}
{-# DEPRECATED fromRelationalDatabaseName "Use generic-lens or generic-optics with 'fromRelationalDatabaseName' instead"  #-}

-- | The Region name and Availability Zone where the database snapshot is located.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsLocation :: Lens.Lens' RelationalDatabaseSnapshot (Core.Maybe Types.ResourceLocation)
rdsLocation = Lens.field @"location"
{-# INLINEABLE rdsLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The name of the database snapshot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsName :: Lens.Lens' RelationalDatabaseSnapshot (Core.Maybe Types.Name)
rdsName = Lens.field @"name"
{-# INLINEABLE rdsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The Lightsail resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsResourceType :: Lens.Lens' RelationalDatabaseSnapshot (Core.Maybe Types.ResourceType)
rdsResourceType = Lens.field @"resourceType"
{-# INLINEABLE rdsResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The size of the disk in GB (for example, @32@ ) for the database snapshot.
--
-- /Note:/ Consider using 'sizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsSizeInGb :: Lens.Lens' RelationalDatabaseSnapshot (Core.Maybe Core.Int)
rdsSizeInGb = Lens.field @"sizeInGb"
{-# INLINEABLE rdsSizeInGb #-}
{-# DEPRECATED sizeInGb "Use generic-lens or generic-optics with 'sizeInGb' instead"  #-}

-- | The state of the database snapshot.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsState :: Lens.Lens' RelationalDatabaseSnapshot (Core.Maybe Types.State)
rdsState = Lens.field @"state"
{-# INLINEABLE rdsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The support code for the database snapshot. Include this code in your email to support when you have questions about a database snapshot in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsSupportCode :: Lens.Lens' RelationalDatabaseSnapshot (Core.Maybe Core.Text)
rdsSupportCode = Lens.field @"supportCode"
{-# INLINEABLE rdsSupportCode #-}
{-# DEPRECATED supportCode "Use generic-lens or generic-optics with 'supportCode' instead"  #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsTags :: Lens.Lens' RelationalDatabaseSnapshot (Core.Maybe [Types.Tag])
rdsTags = Lens.field @"tags"
{-# INLINEABLE rdsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON RelationalDatabaseSnapshot where
        parseJSON
          = Core.withObject "RelationalDatabaseSnapshot" Core.$
              \ x ->
                RelationalDatabaseSnapshot' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "createdAt" Core.<*>
                    x Core..:? "engine"
                    Core.<*> x Core..:? "engineVersion"
                    Core.<*> x Core..:? "fromRelationalDatabaseArn"
                    Core.<*> x Core..:? "fromRelationalDatabaseBlueprintId"
                    Core.<*> x Core..:? "fromRelationalDatabaseBundleId"
                    Core.<*> x Core..:? "fromRelationalDatabaseName"
                    Core.<*> x Core..:? "location"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "resourceType"
                    Core.<*> x Core..:? "sizeInGb"
                    Core.<*> x Core..:? "state"
                    Core.<*> x Core..:? "supportCode"
                    Core.<*> x Core..:? "tags"

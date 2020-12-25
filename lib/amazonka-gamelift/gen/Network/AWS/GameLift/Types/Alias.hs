{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.Alias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.Alias
  ( Alias (..),

    -- * Smart constructor
    mkAlias,

    -- * Lenses
    aAliasArn,
    aAliasId,
    aCreationTime,
    aDescription,
    aLastUpdatedTime,
    aName,
    aRoutingStrategy,
  )
where

import qualified Network.AWS.GameLift.Types.AliasArn as Types
import qualified Network.AWS.GameLift.Types.AliasId as Types
import qualified Network.AWS.GameLift.Types.Description as Types
import qualified Network.AWS.GameLift.Types.Name as Types
import qualified Network.AWS.GameLift.Types.RoutingStrategy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Properties that describe an alias resource.
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
--
--
--
-- /See:/ 'mkAlias' smart constructor.
data Alias = Alias'
  { -- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift alias resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift alias ARN, the resource ID matches the alias ID value.
    aliasArn :: Core.Maybe Types.AliasArn,
    -- | A unique identifier for an alias. Alias IDs are unique within a Region.
    aliasId :: Core.Maybe Types.AliasId,
    -- | A time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | A human-readable description of an alias.
    description :: Core.Maybe Types.Description,
    -- | The time that this data object was last modified. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
    lastUpdatedTime :: Core.Maybe Core.NominalDiffTime,
    -- | A descriptive label that is associated with an alias. Alias names do not need to be unique.
    name :: Core.Maybe Types.Name,
    -- | The routing configuration, including routing type and fleet target, for the alias.
    routingStrategy :: Core.Maybe Types.RoutingStrategy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Alias' value with any optional fields omitted.
mkAlias ::
  Alias
mkAlias =
  Alias'
    { aliasArn = Core.Nothing,
      aliasId = Core.Nothing,
      creationTime = Core.Nothing,
      description = Core.Nothing,
      lastUpdatedTime = Core.Nothing,
      name = Core.Nothing,
      routingStrategy = Core.Nothing
    }

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift alias resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift alias ARN, the resource ID matches the alias ID value.
--
-- /Note:/ Consider using 'aliasArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAliasArn :: Lens.Lens' Alias (Core.Maybe Types.AliasArn)
aAliasArn = Lens.field @"aliasArn"
{-# DEPRECATED aAliasArn "Use generic-lens or generic-optics with 'aliasArn' instead." #-}

-- | A unique identifier for an alias. Alias IDs are unique within a Region.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAliasId :: Lens.Lens' Alias (Core.Maybe Types.AliasId)
aAliasId = Lens.field @"aliasId"
{-# DEPRECATED aAliasId "Use generic-lens or generic-optics with 'aliasId' instead." #-}

-- | A time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCreationTime :: Lens.Lens' Alias (Core.Maybe Core.NominalDiffTime)
aCreationTime = Lens.field @"creationTime"
{-# DEPRECATED aCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A human-readable description of an alias.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDescription :: Lens.Lens' Alias (Core.Maybe Types.Description)
aDescription = Lens.field @"description"
{-# DEPRECATED aDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The time that this data object was last modified. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aLastUpdatedTime :: Lens.Lens' Alias (Core.Maybe Core.NominalDiffTime)
aLastUpdatedTime = Lens.field @"lastUpdatedTime"
{-# DEPRECATED aLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

-- | A descriptive label that is associated with an alias. Alias names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Alias (Core.Maybe Types.Name)
aName = Lens.field @"name"
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The routing configuration, including routing type and fleet target, for the alias.
--
-- /Note:/ Consider using 'routingStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRoutingStrategy :: Lens.Lens' Alias (Core.Maybe Types.RoutingStrategy)
aRoutingStrategy = Lens.field @"routingStrategy"
{-# DEPRECATED aRoutingStrategy "Use generic-lens or generic-optics with 'routingStrategy' instead." #-}

instance Core.FromJSON Alias where
  parseJSON =
    Core.withObject "Alias" Core.$
      \x ->
        Alias'
          Core.<$> (x Core..:? "AliasArn")
          Core.<*> (x Core..:? "AliasId")
          Core.<*> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "LastUpdatedTime")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "RoutingStrategy")

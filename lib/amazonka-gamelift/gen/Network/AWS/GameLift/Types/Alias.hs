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
    aCreationTime,
    aLastUpdatedTime,
    aAliasId,
    aRoutingStrategy,
    aName,
    aAliasARN,
    aDescription,
  )
where

import Network.AWS.GameLift.Types.RoutingStrategy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { creationTime :: Lude.Maybe Lude.Timestamp,
    lastUpdatedTime :: Lude.Maybe Lude.Timestamp,
    aliasId :: Lude.Maybe Lude.Text,
    routingStrategy :: Lude.Maybe RoutingStrategy,
    name :: Lude.Maybe Lude.Text,
    aliasARN :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Alias' with the minimum fields required to make a request.
--
-- * 'aliasARN' - Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift alias resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift alias ARN, the resource ID matches the alias ID value.
-- * 'aliasId' - A unique identifier for an alias. Alias IDs are unique within a Region.
-- * 'creationTime' - A time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
-- * 'description' - A human-readable description of an alias.
-- * 'lastUpdatedTime' - The time that this data object was last modified. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
-- * 'name' - A descriptive label that is associated with an alias. Alias names do not need to be unique.
-- * 'routingStrategy' - The routing configuration, including routing type and fleet target, for the alias.
mkAlias ::
  Alias
mkAlias =
  Alias'
    { creationTime = Lude.Nothing,
      lastUpdatedTime = Lude.Nothing,
      aliasId = Lude.Nothing,
      routingStrategy = Lude.Nothing,
      name = Lude.Nothing,
      aliasARN = Lude.Nothing,
      description = Lude.Nothing
    }

-- | A time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCreationTime :: Lens.Lens' Alias (Lude.Maybe Lude.Timestamp)
aCreationTime = Lens.lens (creationTime :: Alias -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: Alias)
{-# DEPRECATED aCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The time that this data object was last modified. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aLastUpdatedTime :: Lens.Lens' Alias (Lude.Maybe Lude.Timestamp)
aLastUpdatedTime = Lens.lens (lastUpdatedTime :: Alias -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedTime = a} :: Alias)
{-# DEPRECATED aLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

-- | A unique identifier for an alias. Alias IDs are unique within a Region.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAliasId :: Lens.Lens' Alias (Lude.Maybe Lude.Text)
aAliasId = Lens.lens (aliasId :: Alias -> Lude.Maybe Lude.Text) (\s a -> s {aliasId = a} :: Alias)
{-# DEPRECATED aAliasId "Use generic-lens or generic-optics with 'aliasId' instead." #-}

-- | The routing configuration, including routing type and fleet target, for the alias.
--
-- /Note:/ Consider using 'routingStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRoutingStrategy :: Lens.Lens' Alias (Lude.Maybe RoutingStrategy)
aRoutingStrategy = Lens.lens (routingStrategy :: Alias -> Lude.Maybe RoutingStrategy) (\s a -> s {routingStrategy = a} :: Alias)
{-# DEPRECATED aRoutingStrategy "Use generic-lens or generic-optics with 'routingStrategy' instead." #-}

-- | A descriptive label that is associated with an alias. Alias names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Alias (Lude.Maybe Lude.Text)
aName = Lens.lens (name :: Alias -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Alias)
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift alias resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift alias ARN, the resource ID matches the alias ID value.
--
-- /Note:/ Consider using 'aliasARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAliasARN :: Lens.Lens' Alias (Lude.Maybe Lude.Text)
aAliasARN = Lens.lens (aliasARN :: Alias -> Lude.Maybe Lude.Text) (\s a -> s {aliasARN = a} :: Alias)
{-# DEPRECATED aAliasARN "Use generic-lens or generic-optics with 'aliasARN' instead." #-}

-- | A human-readable description of an alias.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDescription :: Lens.Lens' Alias (Lude.Maybe Lude.Text)
aDescription = Lens.lens (description :: Alias -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Alias)
{-# DEPRECATED aDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON Alias where
  parseJSON =
    Lude.withObject
      "Alias"
      ( \x ->
          Alias'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "LastUpdatedTime")
            Lude.<*> (x Lude..:? "AliasId")
            Lude.<*> (x Lude..:? "RoutingStrategy")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "AliasArn")
            Lude.<*> (x Lude..:? "Description")
      )

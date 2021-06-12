{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.Alias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.Alias where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types.RoutingStrategy
import qualified Network.AWS.Lens as Lens

-- | Properties that describe an alias resource.
--
-- -   CreateAlias
--
-- -   ListAliases
--
-- -   DescribeAlias
--
-- -   UpdateAlias
--
-- -   DeleteAlias
--
-- -   ResolveAlias
--
-- /See:/ 'newAlias' smart constructor.
data Alias = Alias'
  { -- | The routing configuration, including routing type and fleet target, for
    -- the alias.
    routingStrategy :: Core.Maybe RoutingStrategy,
    -- | A time stamp indicating when this data object was created. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- \"1469498468.057\").
    creationTime :: Core.Maybe Core.POSIX,
    -- | A descriptive label that is associated with an alias. Alias names do not
    -- need to be unique.
    name :: Core.Maybe Core.Text,
    -- | A human-readable description of an alias.
    description :: Core.Maybe Core.Text,
    -- | Amazon Resource Name
    -- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
    -- that is assigned to a GameLift alias resource and uniquely identifies
    -- it. ARNs are unique across all Regions. In a GameLift alias ARN, the
    -- resource ID matches the alias ID value.
    aliasArn :: Core.Maybe Core.Text,
    -- | A unique identifier for an alias. Alias IDs are unique within a Region.
    aliasId :: Core.Maybe Core.Text,
    -- | The time that this data object was last modified. Format is a number
    -- expressed in Unix time as milliseconds (for example \"1469498468.057\").
    lastUpdatedTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Alias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routingStrategy', 'alias_routingStrategy' - The routing configuration, including routing type and fleet target, for
-- the alias.
--
-- 'creationTime', 'alias_creationTime' - A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
--
-- 'name', 'alias_name' - A descriptive label that is associated with an alias. Alias names do not
-- need to be unique.
--
-- 'description', 'alias_description' - A human-readable description of an alias.
--
-- 'aliasArn', 'alias_aliasArn' - Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- that is assigned to a GameLift alias resource and uniquely identifies
-- it. ARNs are unique across all Regions. In a GameLift alias ARN, the
-- resource ID matches the alias ID value.
--
-- 'aliasId', 'alias_aliasId' - A unique identifier for an alias. Alias IDs are unique within a Region.
--
-- 'lastUpdatedTime', 'alias_lastUpdatedTime' - The time that this data object was last modified. Format is a number
-- expressed in Unix time as milliseconds (for example \"1469498468.057\").
newAlias ::
  Alias
newAlias =
  Alias'
    { routingStrategy = Core.Nothing,
      creationTime = Core.Nothing,
      name = Core.Nothing,
      description = Core.Nothing,
      aliasArn = Core.Nothing,
      aliasId = Core.Nothing,
      lastUpdatedTime = Core.Nothing
    }

-- | The routing configuration, including routing type and fleet target, for
-- the alias.
alias_routingStrategy :: Lens.Lens' Alias (Core.Maybe RoutingStrategy)
alias_routingStrategy = Lens.lens (\Alias' {routingStrategy} -> routingStrategy) (\s@Alias' {} a -> s {routingStrategy = a} :: Alias)

-- | A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
alias_creationTime :: Lens.Lens' Alias (Core.Maybe Core.UTCTime)
alias_creationTime = Lens.lens (\Alias' {creationTime} -> creationTime) (\s@Alias' {} a -> s {creationTime = a} :: Alias) Core.. Lens.mapping Core._Time

-- | A descriptive label that is associated with an alias. Alias names do not
-- need to be unique.
alias_name :: Lens.Lens' Alias (Core.Maybe Core.Text)
alias_name = Lens.lens (\Alias' {name} -> name) (\s@Alias' {} a -> s {name = a} :: Alias)

-- | A human-readable description of an alias.
alias_description :: Lens.Lens' Alias (Core.Maybe Core.Text)
alias_description = Lens.lens (\Alias' {description} -> description) (\s@Alias' {} a -> s {description = a} :: Alias)

-- | Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- that is assigned to a GameLift alias resource and uniquely identifies
-- it. ARNs are unique across all Regions. In a GameLift alias ARN, the
-- resource ID matches the alias ID value.
alias_aliasArn :: Lens.Lens' Alias (Core.Maybe Core.Text)
alias_aliasArn = Lens.lens (\Alias' {aliasArn} -> aliasArn) (\s@Alias' {} a -> s {aliasArn = a} :: Alias)

-- | A unique identifier for an alias. Alias IDs are unique within a Region.
alias_aliasId :: Lens.Lens' Alias (Core.Maybe Core.Text)
alias_aliasId = Lens.lens (\Alias' {aliasId} -> aliasId) (\s@Alias' {} a -> s {aliasId = a} :: Alias)

-- | The time that this data object was last modified. Format is a number
-- expressed in Unix time as milliseconds (for example \"1469498468.057\").
alias_lastUpdatedTime :: Lens.Lens' Alias (Core.Maybe Core.UTCTime)
alias_lastUpdatedTime = Lens.lens (\Alias' {lastUpdatedTime} -> lastUpdatedTime) (\s@Alias' {} a -> s {lastUpdatedTime = a} :: Alias) Core.. Lens.mapping Core._Time

instance Core.FromJSON Alias where
  parseJSON =
    Core.withObject
      "Alias"
      ( \x ->
          Alias'
            Core.<$> (x Core..:? "RoutingStrategy")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "AliasArn")
            Core.<*> (x Core..:? "AliasId")
            Core.<*> (x Core..:? "LastUpdatedTime")
      )

instance Core.Hashable Alias

instance Core.NFData Alias

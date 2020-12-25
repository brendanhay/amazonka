{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.RulesPackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.RulesPackage
  ( RulesPackage (..),

    -- * Smart constructor
    mkRulesPackage,

    -- * Lenses
    rpArn,
    rpName,
    rpVersion,
    rpProvider,
    rpDescription,
  )
where

import qualified Network.AWS.Inspector.Types.Arn as Types
import qualified Network.AWS.Inspector.Types.Description as Types
import qualified Network.AWS.Inspector.Types.Name as Types
import qualified Network.AWS.Inspector.Types.Provider as Types
import qualified Network.AWS.Inspector.Types.Version as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an Amazon Inspector rules package. This data type is used as the response element in the 'DescribeRulesPackages' action.
--
-- /See:/ 'mkRulesPackage' smart constructor.
data RulesPackage = RulesPackage'
  { -- | The ARN of the rules package.
    arn :: Types.Arn,
    -- | The name of the rules package.
    name :: Types.Name,
    -- | The version ID of the rules package.
    version :: Types.Version,
    -- | The provider of the rules package.
    provider :: Types.Provider,
    -- | The description of the rules package.
    description :: Core.Maybe Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RulesPackage' value with any optional fields omitted.
mkRulesPackage ::
  -- | 'arn'
  Types.Arn ->
  -- | 'name'
  Types.Name ->
  -- | 'version'
  Types.Version ->
  -- | 'provider'
  Types.Provider ->
  RulesPackage
mkRulesPackage arn name version provider =
  RulesPackage'
    { arn,
      name,
      version,
      provider,
      description = Core.Nothing
    }

-- | The ARN of the rules package.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpArn :: Lens.Lens' RulesPackage Types.Arn
rpArn = Lens.field @"arn"
{-# DEPRECATED rpArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the rules package.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpName :: Lens.Lens' RulesPackage Types.Name
rpName = Lens.field @"name"
{-# DEPRECATED rpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version ID of the rules package.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpVersion :: Lens.Lens' RulesPackage Types.Version
rpVersion = Lens.field @"version"
{-# DEPRECATED rpVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The provider of the rules package.
--
-- /Note:/ Consider using 'provider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpProvider :: Lens.Lens' RulesPackage Types.Provider
rpProvider = Lens.field @"provider"
{-# DEPRECATED rpProvider "Use generic-lens or generic-optics with 'provider' instead." #-}

-- | The description of the rules package.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpDescription :: Lens.Lens' RulesPackage (Core.Maybe Types.Description)
rpDescription = Lens.field @"description"
{-# DEPRECATED rpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromJSON RulesPackage where
  parseJSON =
    Core.withObject "RulesPackage" Core.$
      \x ->
        RulesPackage'
          Core.<$> (x Core..: "arn")
          Core.<*> (x Core..: "name")
          Core.<*> (x Core..: "version")
          Core.<*> (x Core..: "provider")
          Core.<*> (x Core..:? "description")

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.AliasConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.AliasConfiguration
  ( AliasConfiguration (..)
  -- * Smart constructor
  , mkAliasConfiguration
  -- * Lenses
  , acAliasArn
  , acDescription
  , acFunctionVersion
  , acName
  , acRevisionId
  , acRoutingConfig
  ) where

import qualified Network.AWS.Lambda.Types.Alias as Types
import qualified Network.AWS.Lambda.Types.AliasRoutingConfiguration as Types
import qualified Network.AWS.Lambda.Types.Description as Types
import qualified Network.AWS.Lambda.Types.FunctionArn as Types
import qualified Network.AWS.Lambda.Types.Version as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides configuration information about a Lambda function <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html alias> .
--
-- /See:/ 'mkAliasConfiguration' smart constructor.
data AliasConfiguration = AliasConfiguration'
  { aliasArn :: Core.Maybe Types.FunctionArn
    -- ^ The Amazon Resource Name (ARN) of the alias.
  , description :: Core.Maybe Types.Description
    -- ^ A description of the alias.
  , functionVersion :: Core.Maybe Types.Version
    -- ^ The function version that the alias invokes.
  , name :: Core.Maybe Types.Alias
    -- ^ The name of the alias.
  , revisionId :: Core.Maybe Core.Text
    -- ^ A unique identifier that changes when you update the alias.
  , routingConfig :: Core.Maybe Types.AliasRoutingConfiguration
    -- ^ The <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html routing configuration> of the alias.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AliasConfiguration' value with any optional fields omitted.
mkAliasConfiguration
    :: AliasConfiguration
mkAliasConfiguration
  = AliasConfiguration'{aliasArn = Core.Nothing,
                        description = Core.Nothing, functionVersion = Core.Nothing,
                        name = Core.Nothing, revisionId = Core.Nothing,
                        routingConfig = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the alias.
--
-- /Note:/ Consider using 'aliasArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acAliasArn :: Lens.Lens' AliasConfiguration (Core.Maybe Types.FunctionArn)
acAliasArn = Lens.field @"aliasArn"
{-# INLINEABLE acAliasArn #-}
{-# DEPRECATED aliasArn "Use generic-lens or generic-optics with 'aliasArn' instead"  #-}

-- | A description of the alias.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acDescription :: Lens.Lens' AliasConfiguration (Core.Maybe Types.Description)
acDescription = Lens.field @"description"
{-# INLINEABLE acDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The function version that the alias invokes.
--
-- /Note:/ Consider using 'functionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acFunctionVersion :: Lens.Lens' AliasConfiguration (Core.Maybe Types.Version)
acFunctionVersion = Lens.field @"functionVersion"
{-# INLINEABLE acFunctionVersion #-}
{-# DEPRECATED functionVersion "Use generic-lens or generic-optics with 'functionVersion' instead"  #-}

-- | The name of the alias.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acName :: Lens.Lens' AliasConfiguration (Core.Maybe Types.Alias)
acName = Lens.field @"name"
{-# INLINEABLE acName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A unique identifier that changes when you update the alias.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acRevisionId :: Lens.Lens' AliasConfiguration (Core.Maybe Core.Text)
acRevisionId = Lens.field @"revisionId"
{-# INLINEABLE acRevisionId #-}
{-# DEPRECATED revisionId "Use generic-lens or generic-optics with 'revisionId' instead"  #-}

-- | The <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html routing configuration> of the alias.
--
-- /Note:/ Consider using 'routingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acRoutingConfig :: Lens.Lens' AliasConfiguration (Core.Maybe Types.AliasRoutingConfiguration)
acRoutingConfig = Lens.field @"routingConfig"
{-# INLINEABLE acRoutingConfig #-}
{-# DEPRECATED routingConfig "Use generic-lens or generic-optics with 'routingConfig' instead"  #-}

instance Core.FromJSON AliasConfiguration where
        parseJSON
          = Core.withObject "AliasConfiguration" Core.$
              \ x ->
                AliasConfiguration' Core.<$>
                  (x Core..:? "AliasArn") Core.<*> x Core..:? "Description" Core.<*>
                    x Core..:? "FunctionVersion"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "RevisionId"
                    Core.<*> x Core..:? "RoutingConfig"

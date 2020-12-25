{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.CodeSigningConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.CodeSigningConfig
  ( CodeSigningConfig (..),

    -- * Smart constructor
    mkCodeSigningConfig,

    -- * Lenses
    cscCodeSigningConfigId,
    cscCodeSigningConfigArn,
    cscAllowedPublishers,
    cscCodeSigningPolicies,
    cscLastModified,
    cscDescription,
  )
where

import qualified Network.AWS.Lambda.Types.AllowedPublishers as Types
import qualified Network.AWS.Lambda.Types.CodeSigningConfigArn as Types
import qualified Network.AWS.Lambda.Types.CodeSigningConfigId as Types
import qualified Network.AWS.Lambda.Types.CodeSigningPolicies as Types
import qualified Network.AWS.Lambda.Types.Description as Types
import qualified Network.AWS.Lambda.Types.LastModified as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about a Code signing configuration.
--
-- /See:/ 'mkCodeSigningConfig' smart constructor.
data CodeSigningConfig = CodeSigningConfig'
  { -- | Unique identifer for the Code signing configuration.
    codeSigningConfigId :: Types.CodeSigningConfigId,
    -- | The Amazon Resource Name (ARN) of the Code signing configuration.
    codeSigningConfigArn :: Types.CodeSigningConfigArn,
    -- | List of allowed publishers.
    allowedPublishers :: Types.AllowedPublishers,
    -- | The code signing policy controls the validation failure action for signature mismatch or expiry.
    codeSigningPolicies :: Types.CodeSigningPolicies,
    -- | The date and time that the Code signing configuration was last modified, in ISO-8601 format (YYYY-MM-DDThh:mm:ss.sTZD).
    lastModified :: Types.LastModified,
    -- | Code signing configuration description.
    description :: Core.Maybe Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CodeSigningConfig' value with any optional fields omitted.
mkCodeSigningConfig ::
  -- | 'codeSigningConfigId'
  Types.CodeSigningConfigId ->
  -- | 'codeSigningConfigArn'
  Types.CodeSigningConfigArn ->
  -- | 'allowedPublishers'
  Types.AllowedPublishers ->
  -- | 'codeSigningPolicies'
  Types.CodeSigningPolicies ->
  -- | 'lastModified'
  Types.LastModified ->
  CodeSigningConfig
mkCodeSigningConfig
  codeSigningConfigId
  codeSigningConfigArn
  allowedPublishers
  codeSigningPolicies
  lastModified =
    CodeSigningConfig'
      { codeSigningConfigId,
        codeSigningConfigArn,
        allowedPublishers,
        codeSigningPolicies,
        lastModified,
        description = Core.Nothing
      }

-- | Unique identifer for the Code signing configuration.
--
-- /Note:/ Consider using 'codeSigningConfigId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscCodeSigningConfigId :: Lens.Lens' CodeSigningConfig Types.CodeSigningConfigId
cscCodeSigningConfigId = Lens.field @"codeSigningConfigId"
{-# DEPRECATED cscCodeSigningConfigId "Use generic-lens or generic-optics with 'codeSigningConfigId' instead." #-}

-- | The Amazon Resource Name (ARN) of the Code signing configuration.
--
-- /Note:/ Consider using 'codeSigningConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscCodeSigningConfigArn :: Lens.Lens' CodeSigningConfig Types.CodeSigningConfigArn
cscCodeSigningConfigArn = Lens.field @"codeSigningConfigArn"
{-# DEPRECATED cscCodeSigningConfigArn "Use generic-lens or generic-optics with 'codeSigningConfigArn' instead." #-}

-- | List of allowed publishers.
--
-- /Note:/ Consider using 'allowedPublishers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscAllowedPublishers :: Lens.Lens' CodeSigningConfig Types.AllowedPublishers
cscAllowedPublishers = Lens.field @"allowedPublishers"
{-# DEPRECATED cscAllowedPublishers "Use generic-lens or generic-optics with 'allowedPublishers' instead." #-}

-- | The code signing policy controls the validation failure action for signature mismatch or expiry.
--
-- /Note:/ Consider using 'codeSigningPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscCodeSigningPolicies :: Lens.Lens' CodeSigningConfig Types.CodeSigningPolicies
cscCodeSigningPolicies = Lens.field @"codeSigningPolicies"
{-# DEPRECATED cscCodeSigningPolicies "Use generic-lens or generic-optics with 'codeSigningPolicies' instead." #-}

-- | The date and time that the Code signing configuration was last modified, in ISO-8601 format (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscLastModified :: Lens.Lens' CodeSigningConfig Types.LastModified
cscLastModified = Lens.field @"lastModified"
{-# DEPRECATED cscLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | Code signing configuration description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscDescription :: Lens.Lens' CodeSigningConfig (Core.Maybe Types.Description)
cscDescription = Lens.field @"description"
{-# DEPRECATED cscDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromJSON CodeSigningConfig where
  parseJSON =
    Core.withObject "CodeSigningConfig" Core.$
      \x ->
        CodeSigningConfig'
          Core.<$> (x Core..: "CodeSigningConfigId")
          Core.<*> (x Core..: "CodeSigningConfigArn")
          Core.<*> (x Core..: "AllowedPublishers")
          Core.<*> (x Core..: "CodeSigningPolicies")
          Core.<*> (x Core..: "LastModified")
          Core.<*> (x Core..:? "Description")

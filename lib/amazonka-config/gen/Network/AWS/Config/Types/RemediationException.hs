{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationException
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationException
  ( RemediationException (..),

    -- * Smart constructor
    mkRemediationException,

    -- * Lenses
    reConfigRuleName,
    reResourceType,
    reResourceId,
    reExpirationTime,
    reMessage,
  )
where

import qualified Network.AWS.Config.Types.ConfigRuleName as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit1024 as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that represents the details about the remediation exception. The details include the rule name, an explanation of an exception, the time when the exception will be deleted, the resource ID, and resource type.
--
-- /See:/ 'mkRemediationException' smart constructor.
data RemediationException = RemediationException'
  { -- | The name of the AWS Config rule.
    configRuleName :: Types.ConfigRuleName,
    -- | The type of a resource.
    resourceType :: Types.StringWithCharLimit256,
    -- | The ID of the resource (for example., sg-xxxxxx).
    resourceId :: Types.StringWithCharLimit1024,
    -- | The time when the remediation exception will be deleted.
    expirationTime :: Core.Maybe Core.NominalDiffTime,
    -- | An explanation of an remediation exception.
    message :: Core.Maybe Types.StringWithCharLimit1024
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RemediationException' value with any optional fields omitted.
mkRemediationException ::
  -- | 'configRuleName'
  Types.ConfigRuleName ->
  -- | 'resourceType'
  Types.StringWithCharLimit256 ->
  -- | 'resourceId'
  Types.StringWithCharLimit1024 ->
  RemediationException
mkRemediationException configRuleName resourceType resourceId =
  RemediationException'
    { configRuleName,
      resourceType,
      resourceId,
      expirationTime = Core.Nothing,
      message = Core.Nothing
    }

-- | The name of the AWS Config rule.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reConfigRuleName :: Lens.Lens' RemediationException Types.ConfigRuleName
reConfigRuleName = Lens.field @"configRuleName"
{-# DEPRECATED reConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | The type of a resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reResourceType :: Lens.Lens' RemediationException Types.StringWithCharLimit256
reResourceType = Lens.field @"resourceType"
{-# DEPRECATED reResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The ID of the resource (for example., sg-xxxxxx).
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reResourceId :: Lens.Lens' RemediationException Types.StringWithCharLimit1024
reResourceId = Lens.field @"resourceId"
{-# DEPRECATED reResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The time when the remediation exception will be deleted.
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reExpirationTime :: Lens.Lens' RemediationException (Core.Maybe Core.NominalDiffTime)
reExpirationTime = Lens.field @"expirationTime"
{-# DEPRECATED reExpirationTime "Use generic-lens or generic-optics with 'expirationTime' instead." #-}

-- | An explanation of an remediation exception.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reMessage :: Lens.Lens' RemediationException (Core.Maybe Types.StringWithCharLimit1024)
reMessage = Lens.field @"message"
{-# DEPRECATED reMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromJSON RemediationException where
  parseJSON =
    Core.withObject "RemediationException" Core.$
      \x ->
        RemediationException'
          Core.<$> (x Core..: "ConfigRuleName")
          Core.<*> (x Core..: "ResourceType")
          Core.<*> (x Core..: "ResourceId")
          Core.<*> (x Core..:? "ExpirationTime")
          Core.<*> (x Core..:? "Message")

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ValidationMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ValidationMessage
  ( ValidationMessage (..),

    -- * Smart constructor
    mkValidationMessage,

    -- * Lenses
    vmMessage,
    vmNamespace,
    vmOptionName,
    vmSeverity,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionName as Types
import qualified Network.AWS.ElasticBeanstalk.Types.OptionNamespace as Types
import qualified Network.AWS.ElasticBeanstalk.Types.ValidationMessageString as Types
import qualified Network.AWS.ElasticBeanstalk.Types.ValidationSeverity as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An error or warning for a desired configuration option value.
--
-- /See:/ 'mkValidationMessage' smart constructor.
data ValidationMessage = ValidationMessage'
  { -- | A message describing the error or warning.
    message :: Core.Maybe Types.ValidationMessageString,
    -- | The namespace to which the option belongs.
    namespace :: Core.Maybe Types.OptionNamespace,
    -- | The name of the option.
    optionName :: Core.Maybe Types.ConfigurationOptionName,
    -- | An indication of the severity of this message:
    --
    --
    --     * @error@ : This message indicates that this is not a valid setting for an option.
    --
    --
    --     * @warning@ : This message is providing information you should take into account.
    severity :: Core.Maybe Types.ValidationSeverity
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ValidationMessage' value with any optional fields omitted.
mkValidationMessage ::
  ValidationMessage
mkValidationMessage =
  ValidationMessage'
    { message = Core.Nothing,
      namespace = Core.Nothing,
      optionName = Core.Nothing,
      severity = Core.Nothing
    }

-- | A message describing the error or warning.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmMessage :: Lens.Lens' ValidationMessage (Core.Maybe Types.ValidationMessageString)
vmMessage = Lens.field @"message"
{-# DEPRECATED vmMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The namespace to which the option belongs.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmNamespace :: Lens.Lens' ValidationMessage (Core.Maybe Types.OptionNamespace)
vmNamespace = Lens.field @"namespace"
{-# DEPRECATED vmNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The name of the option.
--
-- /Note:/ Consider using 'optionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmOptionName :: Lens.Lens' ValidationMessage (Core.Maybe Types.ConfigurationOptionName)
vmOptionName = Lens.field @"optionName"
{-# DEPRECATED vmOptionName "Use generic-lens or generic-optics with 'optionName' instead." #-}

-- | An indication of the severity of this message:
--
--
--     * @error@ : This message indicates that this is not a valid setting for an option.
--
--
--     * @warning@ : This message is providing information you should take into account.
--
--
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmSeverity :: Lens.Lens' ValidationMessage (Core.Maybe Types.ValidationSeverity)
vmSeverity = Lens.field @"severity"
{-# DEPRECATED vmSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

instance Core.FromXML ValidationMessage where
  parseXML x =
    ValidationMessage'
      Core.<$> (x Core..@? "Message")
      Core.<*> (x Core..@? "Namespace")
      Core.<*> (x Core..@? "OptionName")
      Core.<*> (x Core..@? "Severity")

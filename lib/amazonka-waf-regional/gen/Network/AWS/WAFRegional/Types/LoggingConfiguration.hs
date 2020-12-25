{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.LoggingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.LoggingConfiguration
  ( LoggingConfiguration (..),

    -- * Smart constructor
    mkLoggingConfiguration,

    -- * Lenses
    lcResourceArn,
    lcLogDestinationConfigs,
    lcRedactedFields,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.FieldToMatch as Types
import qualified Network.AWS.WAFRegional.Types.ResourceArn as Types

-- | The Amazon Kinesis Data Firehose, @RedactedFields@ information, and the web ACL Amazon Resource Name (ARN).
--
-- /See:/ 'mkLoggingConfiguration' smart constructor.
data LoggingConfiguration = LoggingConfiguration'
  { -- | The Amazon Resource Name (ARN) of the web ACL that you want to associate with @LogDestinationConfigs@ .
    resourceArn :: Types.ResourceArn,
    -- | An array of Amazon Kinesis Data Firehose ARNs.
    logDestinationConfigs :: Core.NonEmpty Types.ResourceArn,
    -- | The parts of the request that you want redacted from the logs. For example, if you redact the cookie field, the cookie field in the firehose will be @xxx@ .
    redactedFields :: Core.Maybe [Types.FieldToMatch]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoggingConfiguration' value with any optional fields omitted.
mkLoggingConfiguration ::
  -- | 'resourceArn'
  Types.ResourceArn ->
  -- | 'logDestinationConfigs'
  Core.NonEmpty Types.ResourceArn ->
  LoggingConfiguration
mkLoggingConfiguration resourceArn logDestinationConfigs =
  LoggingConfiguration'
    { resourceArn,
      logDestinationConfigs,
      redactedFields = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the web ACL that you want to associate with @LogDestinationConfigs@ .
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcResourceArn :: Lens.Lens' LoggingConfiguration Types.ResourceArn
lcResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED lcResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

-- | An array of Amazon Kinesis Data Firehose ARNs.
--
-- /Note:/ Consider using 'logDestinationConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcLogDestinationConfigs :: Lens.Lens' LoggingConfiguration (Core.NonEmpty Types.ResourceArn)
lcLogDestinationConfigs = Lens.field @"logDestinationConfigs"
{-# DEPRECATED lcLogDestinationConfigs "Use generic-lens or generic-optics with 'logDestinationConfigs' instead." #-}

-- | The parts of the request that you want redacted from the logs. For example, if you redact the cookie field, the cookie field in the firehose will be @xxx@ .
--
-- /Note:/ Consider using 'redactedFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcRedactedFields :: Lens.Lens' LoggingConfiguration (Core.Maybe [Types.FieldToMatch])
lcRedactedFields = Lens.field @"redactedFields"
{-# DEPRECATED lcRedactedFields "Use generic-lens or generic-optics with 'redactedFields' instead." #-}

instance Core.FromJSON LoggingConfiguration where
  toJSON LoggingConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceArn" Core..= resourceArn),
            Core.Just ("LogDestinationConfigs" Core..= logDestinationConfigs),
            ("RedactedFields" Core..=) Core.<$> redactedFields
          ]
      )

instance Core.FromJSON LoggingConfiguration where
  parseJSON =
    Core.withObject "LoggingConfiguration" Core.$
      \x ->
        LoggingConfiguration'
          Core.<$> (x Core..: "ResourceArn")
          Core.<*> (x Core..: "LogDestinationConfigs")
          Core.<*> (x Core..:? "RedactedFields")

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.SSMValidationParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.SSMValidationParameters
  ( SSMValidationParameters (..),

    -- * Smart constructor
    mkSSMValidationParameters,

    -- * Lenses
    ssmvpCommand,
    ssmvpExecutionTimeoutSeconds,
    ssmvpInstanceId,
    ssmvpOutputS3BucketName,
    ssmvpScriptType,
    ssmvpSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.BucketName as Types
import qualified Network.AWS.SMS.Types.Command as Types
import qualified Network.AWS.SMS.Types.InstanceId as Types
import qualified Network.AWS.SMS.Types.ScriptType as Types
import qualified Network.AWS.SMS.Types.Source as Types

-- | Contains validation parameters.
--
-- /See:/ 'mkSSMValidationParameters' smart constructor.
data SSMValidationParameters = SSMValidationParameters'
  { -- | The command to run the validation script
    command :: Core.Maybe Types.Command,
    -- | The timeout interval, in seconds.
    executionTimeoutSeconds :: Core.Maybe Core.Natural,
    -- | The ID of the instance. The instance must have the following tag: UserForSMSApplicationValidation=true.
    instanceId :: Core.Maybe Types.InstanceId,
    -- | The name of the S3 bucket for output.
    outputS3BucketName :: Core.Maybe Types.BucketName,
    -- | The type of validation script.
    scriptType :: Core.Maybe Types.ScriptType,
    -- | The location of the validation script.
    source :: Core.Maybe Types.Source
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SSMValidationParameters' value with any optional fields omitted.
mkSSMValidationParameters ::
  SSMValidationParameters
mkSSMValidationParameters =
  SSMValidationParameters'
    { command = Core.Nothing,
      executionTimeoutSeconds = Core.Nothing,
      instanceId = Core.Nothing,
      outputS3BucketName = Core.Nothing,
      scriptType = Core.Nothing,
      source = Core.Nothing
    }

-- | The command to run the validation script
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmvpCommand :: Lens.Lens' SSMValidationParameters (Core.Maybe Types.Command)
ssmvpCommand = Lens.field @"command"
{-# DEPRECATED ssmvpCommand "Use generic-lens or generic-optics with 'command' instead." #-}

-- | The timeout interval, in seconds.
--
-- /Note:/ Consider using 'executionTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmvpExecutionTimeoutSeconds :: Lens.Lens' SSMValidationParameters (Core.Maybe Core.Natural)
ssmvpExecutionTimeoutSeconds = Lens.field @"executionTimeoutSeconds"
{-# DEPRECATED ssmvpExecutionTimeoutSeconds "Use generic-lens or generic-optics with 'executionTimeoutSeconds' instead." #-}

-- | The ID of the instance. The instance must have the following tag: UserForSMSApplicationValidation=true.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmvpInstanceId :: Lens.Lens' SSMValidationParameters (Core.Maybe Types.InstanceId)
ssmvpInstanceId = Lens.field @"instanceId"
{-# DEPRECATED ssmvpInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The name of the S3 bucket for output.
--
-- /Note:/ Consider using 'outputS3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmvpOutputS3BucketName :: Lens.Lens' SSMValidationParameters (Core.Maybe Types.BucketName)
ssmvpOutputS3BucketName = Lens.field @"outputS3BucketName"
{-# DEPRECATED ssmvpOutputS3BucketName "Use generic-lens or generic-optics with 'outputS3BucketName' instead." #-}

-- | The type of validation script.
--
-- /Note:/ Consider using 'scriptType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmvpScriptType :: Lens.Lens' SSMValidationParameters (Core.Maybe Types.ScriptType)
ssmvpScriptType = Lens.field @"scriptType"
{-# DEPRECATED ssmvpScriptType "Use generic-lens or generic-optics with 'scriptType' instead." #-}

-- | The location of the validation script.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmvpSource :: Lens.Lens' SSMValidationParameters (Core.Maybe Types.Source)
ssmvpSource = Lens.field @"source"
{-# DEPRECATED ssmvpSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Core.FromJSON SSMValidationParameters where
  toJSON SSMValidationParameters {..} =
    Core.object
      ( Core.catMaybes
          [ ("command" Core..=) Core.<$> command,
            ("executionTimeoutSeconds" Core..=)
              Core.<$> executionTimeoutSeconds,
            ("instanceId" Core..=) Core.<$> instanceId,
            ("outputS3BucketName" Core..=) Core.<$> outputS3BucketName,
            ("scriptType" Core..=) Core.<$> scriptType,
            ("source" Core..=) Core.<$> source
          ]
      )

instance Core.FromJSON SSMValidationParameters where
  parseJSON =
    Core.withObject "SSMValidationParameters" Core.$
      \x ->
        SSMValidationParameters'
          Core.<$> (x Core..:? "command")
          Core.<*> (x Core..:? "executionTimeoutSeconds")
          Core.<*> (x Core..:? "instanceId")
          Core.<*> (x Core..:? "outputS3BucketName")
          Core.<*> (x Core..:? "scriptType")
          Core.<*> (x Core..:? "source")

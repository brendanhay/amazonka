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
    ssmvpInstanceId,
    ssmvpCommand,
    ssmvpExecutionTimeoutSeconds,
    ssmvpScriptType,
    ssmvpSource,
    ssmvpOutputS3BucketName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.ScriptType
import Network.AWS.SMS.Types.Source

-- | Contains validation parameters.
--
-- /See:/ 'mkSSMValidationParameters' smart constructor.
data SSMValidationParameters = SSMValidationParameters'
  { -- | The ID of the instance. The instance must have the following tag: UserForSMSApplicationValidation=true.
    instanceId :: Lude.Maybe Lude.Text,
    -- | The command to run the validation script
    command :: Lude.Maybe Lude.Text,
    -- | The timeout interval, in seconds.
    executionTimeoutSeconds :: Lude.Maybe Lude.Natural,
    -- | The type of validation script.
    scriptType :: Lude.Maybe ScriptType,
    -- | The location of the validation script.
    source :: Lude.Maybe Source,
    -- | The name of the S3 bucket for output.
    outputS3BucketName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SSMValidationParameters' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance. The instance must have the following tag: UserForSMSApplicationValidation=true.
-- * 'command' - The command to run the validation script
-- * 'executionTimeoutSeconds' - The timeout interval, in seconds.
-- * 'scriptType' - The type of validation script.
-- * 'source' - The location of the validation script.
-- * 'outputS3BucketName' - The name of the S3 bucket for output.
mkSSMValidationParameters ::
  SSMValidationParameters
mkSSMValidationParameters =
  SSMValidationParameters'
    { instanceId = Lude.Nothing,
      command = Lude.Nothing,
      executionTimeoutSeconds = Lude.Nothing,
      scriptType = Lude.Nothing,
      source = Lude.Nothing,
      outputS3BucketName = Lude.Nothing
    }

-- | The ID of the instance. The instance must have the following tag: UserForSMSApplicationValidation=true.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmvpInstanceId :: Lens.Lens' SSMValidationParameters (Lude.Maybe Lude.Text)
ssmvpInstanceId = Lens.lens (instanceId :: SSMValidationParameters -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: SSMValidationParameters)
{-# DEPRECATED ssmvpInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The command to run the validation script
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmvpCommand :: Lens.Lens' SSMValidationParameters (Lude.Maybe Lude.Text)
ssmvpCommand = Lens.lens (command :: SSMValidationParameters -> Lude.Maybe Lude.Text) (\s a -> s {command = a} :: SSMValidationParameters)
{-# DEPRECATED ssmvpCommand "Use generic-lens or generic-optics with 'command' instead." #-}

-- | The timeout interval, in seconds.
--
-- /Note:/ Consider using 'executionTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmvpExecutionTimeoutSeconds :: Lens.Lens' SSMValidationParameters (Lude.Maybe Lude.Natural)
ssmvpExecutionTimeoutSeconds = Lens.lens (executionTimeoutSeconds :: SSMValidationParameters -> Lude.Maybe Lude.Natural) (\s a -> s {executionTimeoutSeconds = a} :: SSMValidationParameters)
{-# DEPRECATED ssmvpExecutionTimeoutSeconds "Use generic-lens or generic-optics with 'executionTimeoutSeconds' instead." #-}

-- | The type of validation script.
--
-- /Note:/ Consider using 'scriptType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmvpScriptType :: Lens.Lens' SSMValidationParameters (Lude.Maybe ScriptType)
ssmvpScriptType = Lens.lens (scriptType :: SSMValidationParameters -> Lude.Maybe ScriptType) (\s a -> s {scriptType = a} :: SSMValidationParameters)
{-# DEPRECATED ssmvpScriptType "Use generic-lens or generic-optics with 'scriptType' instead." #-}

-- | The location of the validation script.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmvpSource :: Lens.Lens' SSMValidationParameters (Lude.Maybe Source)
ssmvpSource = Lens.lens (source :: SSMValidationParameters -> Lude.Maybe Source) (\s a -> s {source = a} :: SSMValidationParameters)
{-# DEPRECATED ssmvpSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The name of the S3 bucket for output.
--
-- /Note:/ Consider using 'outputS3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmvpOutputS3BucketName :: Lens.Lens' SSMValidationParameters (Lude.Maybe Lude.Text)
ssmvpOutputS3BucketName = Lens.lens (outputS3BucketName :: SSMValidationParameters -> Lude.Maybe Lude.Text) (\s a -> s {outputS3BucketName = a} :: SSMValidationParameters)
{-# DEPRECATED ssmvpOutputS3BucketName "Use generic-lens or generic-optics with 'outputS3BucketName' instead." #-}

instance Lude.FromJSON SSMValidationParameters where
  parseJSON =
    Lude.withObject
      "SSMValidationParameters"
      ( \x ->
          SSMValidationParameters'
            Lude.<$> (x Lude..:? "instanceId")
            Lude.<*> (x Lude..:? "command")
            Lude.<*> (x Lude..:? "executionTimeoutSeconds")
            Lude.<*> (x Lude..:? "scriptType")
            Lude.<*> (x Lude..:? "source")
            Lude.<*> (x Lude..:? "outputS3BucketName")
      )

instance Lude.ToJSON SSMValidationParameters where
  toJSON SSMValidationParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("instanceId" Lude..=) Lude.<$> instanceId,
            ("command" Lude..=) Lude.<$> command,
            ("executionTimeoutSeconds" Lude..=)
              Lude.<$> executionTimeoutSeconds,
            ("scriptType" Lude..=) Lude.<$> scriptType,
            ("source" Lude..=) Lude.<$> source,
            ("outputS3BucketName" Lude..=) Lude.<$> outputS3BucketName
          ]
      )

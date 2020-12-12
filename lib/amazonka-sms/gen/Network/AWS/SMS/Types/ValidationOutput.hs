{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ValidationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ValidationOutput
  ( ValidationOutput (..),

    -- * Smart constructor
    mkValidationOutput,

    -- * Lenses
    voStatus,
    voAppValidationOutput,
    voLatestValidationTime,
    voName,
    voStatusMessage,
    voValidationId,
    voServerValidationOutput,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.AppValidationOutput
import Network.AWS.SMS.Types.ServerValidationOutput
import Network.AWS.SMS.Types.ValidationStatus

-- | Contains validation output.
--
-- /See:/ 'mkValidationOutput' smart constructor.
data ValidationOutput = ValidationOutput'
  { status ::
      Lude.Maybe ValidationStatus,
    appValidationOutput :: Lude.Maybe AppValidationOutput,
    latestValidationTime :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    statusMessage :: Lude.Maybe Lude.Text,
    validationId :: Lude.Maybe Lude.Text,
    serverValidationOutput ::
      Lude.Maybe ServerValidationOutput
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ValidationOutput' with the minimum fields required to make a request.
--
-- * 'appValidationOutput' - The output from validating an application.
-- * 'latestValidationTime' - The latest time that the validation was performed.
-- * 'name' - The name of the validation.
-- * 'serverValidationOutput' - The output from validation an instance.
-- * 'status' - The status of the validation.
-- * 'statusMessage' - The status message.
-- * 'validationId' - The ID of the validation.
mkValidationOutput ::
  ValidationOutput
mkValidationOutput =
  ValidationOutput'
    { status = Lude.Nothing,
      appValidationOutput = Lude.Nothing,
      latestValidationTime = Lude.Nothing,
      name = Lude.Nothing,
      statusMessage = Lude.Nothing,
      validationId = Lude.Nothing,
      serverValidationOutput = Lude.Nothing
    }

-- | The status of the validation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
voStatus :: Lens.Lens' ValidationOutput (Lude.Maybe ValidationStatus)
voStatus = Lens.lens (status :: ValidationOutput -> Lude.Maybe ValidationStatus) (\s a -> s {status = a} :: ValidationOutput)
{-# DEPRECATED voStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The output from validating an application.
--
-- /Note:/ Consider using 'appValidationOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
voAppValidationOutput :: Lens.Lens' ValidationOutput (Lude.Maybe AppValidationOutput)
voAppValidationOutput = Lens.lens (appValidationOutput :: ValidationOutput -> Lude.Maybe AppValidationOutput) (\s a -> s {appValidationOutput = a} :: ValidationOutput)
{-# DEPRECATED voAppValidationOutput "Use generic-lens or generic-optics with 'appValidationOutput' instead." #-}

-- | The latest time that the validation was performed.
--
-- /Note:/ Consider using 'latestValidationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
voLatestValidationTime :: Lens.Lens' ValidationOutput (Lude.Maybe Lude.Timestamp)
voLatestValidationTime = Lens.lens (latestValidationTime :: ValidationOutput -> Lude.Maybe Lude.Timestamp) (\s a -> s {latestValidationTime = a} :: ValidationOutput)
{-# DEPRECATED voLatestValidationTime "Use generic-lens or generic-optics with 'latestValidationTime' instead." #-}

-- | The name of the validation.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
voName :: Lens.Lens' ValidationOutput (Lude.Maybe Lude.Text)
voName = Lens.lens (name :: ValidationOutput -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ValidationOutput)
{-# DEPRECATED voName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The status message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
voStatusMessage :: Lens.Lens' ValidationOutput (Lude.Maybe Lude.Text)
voStatusMessage = Lens.lens (statusMessage :: ValidationOutput -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: ValidationOutput)
{-# DEPRECATED voStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The ID of the validation.
--
-- /Note:/ Consider using 'validationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
voValidationId :: Lens.Lens' ValidationOutput (Lude.Maybe Lude.Text)
voValidationId = Lens.lens (validationId :: ValidationOutput -> Lude.Maybe Lude.Text) (\s a -> s {validationId = a} :: ValidationOutput)
{-# DEPRECATED voValidationId "Use generic-lens or generic-optics with 'validationId' instead." #-}

-- | The output from validation an instance.
--
-- /Note:/ Consider using 'serverValidationOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
voServerValidationOutput :: Lens.Lens' ValidationOutput (Lude.Maybe ServerValidationOutput)
voServerValidationOutput = Lens.lens (serverValidationOutput :: ValidationOutput -> Lude.Maybe ServerValidationOutput) (\s a -> s {serverValidationOutput = a} :: ValidationOutput)
{-# DEPRECATED voServerValidationOutput "Use generic-lens or generic-optics with 'serverValidationOutput' instead." #-}

instance Lude.FromJSON ValidationOutput where
  parseJSON =
    Lude.withObject
      "ValidationOutput"
      ( \x ->
          ValidationOutput'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "appValidationOutput")
            Lude.<*> (x Lude..:? "latestValidationTime")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "statusMessage")
            Lude.<*> (x Lude..:? "validationId")
            Lude.<*> (x Lude..:? "serverValidationOutput")
      )

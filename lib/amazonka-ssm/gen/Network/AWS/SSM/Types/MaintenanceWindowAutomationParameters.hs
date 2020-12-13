{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowAutomationParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowAutomationParameters
  ( MaintenanceWindowAutomationParameters (..),

    -- * Smart constructor
    mkMaintenanceWindowAutomationParameters,

    -- * Lenses
    mwapParameters,
    mwapDocumentVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The parameters for an AUTOMATION task type.
--
-- /See:/ 'mkMaintenanceWindowAutomationParameters' smart constructor.
data MaintenanceWindowAutomationParameters = MaintenanceWindowAutomationParameters'
  { -- | The parameters for the AUTOMATION task.
    --
    -- For information about specifying and updating task parameters, see 'RegisterTaskWithMaintenanceWindow' and 'UpdateMaintenanceWindowTask' .
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    -- | The version of an Automation document to use during task execution.
    documentVersion :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MaintenanceWindowAutomationParameters' with the minimum fields required to make a request.
--
-- * 'parameters' - The parameters for the AUTOMATION task.
--
-- For information about specifying and updating task parameters, see 'RegisterTaskWithMaintenanceWindow' and 'UpdateMaintenanceWindowTask' .
-- * 'documentVersion' - The version of an Automation document to use during task execution.
mkMaintenanceWindowAutomationParameters ::
  MaintenanceWindowAutomationParameters
mkMaintenanceWindowAutomationParameters =
  MaintenanceWindowAutomationParameters'
    { parameters = Lude.Nothing,
      documentVersion = Lude.Nothing
    }

-- | The parameters for the AUTOMATION task.
--
-- For information about specifying and updating task parameters, see 'RegisterTaskWithMaintenanceWindow' and 'UpdateMaintenanceWindowTask' .
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwapParameters :: Lens.Lens' MaintenanceWindowAutomationParameters (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
mwapParameters = Lens.lens (parameters :: MaintenanceWindowAutomationParameters -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {parameters = a} :: MaintenanceWindowAutomationParameters)
{-# DEPRECATED mwapParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The version of an Automation document to use during task execution.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwapDocumentVersion :: Lens.Lens' MaintenanceWindowAutomationParameters (Lude.Maybe Lude.Text)
mwapDocumentVersion = Lens.lens (documentVersion :: MaintenanceWindowAutomationParameters -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: MaintenanceWindowAutomationParameters)
{-# DEPRECATED mwapDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

instance Lude.FromJSON MaintenanceWindowAutomationParameters where
  parseJSON =
    Lude.withObject
      "MaintenanceWindowAutomationParameters"
      ( \x ->
          MaintenanceWindowAutomationParameters'
            Lude.<$> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "DocumentVersion")
      )

instance Lude.ToJSON MaintenanceWindowAutomationParameters where
  toJSON MaintenanceWindowAutomationParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Parameters" Lude..=) Lude.<$> parameters,
            ("DocumentVersion" Lude..=) Lude.<$> documentVersion
          ]
      )

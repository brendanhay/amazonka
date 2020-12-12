{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.TelemetryConfigurationUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.TelemetryConfigurationUpdate
  ( TelemetryConfigurationUpdate (..),

    -- * Smart constructor
    mkTelemetryConfigurationUpdate,

    -- * Lenses
    tcuTelemetry,
  )
where

import Network.AWS.Greengrass.Types.Telemetry
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration settings for running telemetry.
--
-- /See:/ 'mkTelemetryConfigurationUpdate' smart constructor.
newtype TelemetryConfigurationUpdate = TelemetryConfigurationUpdate'
  { telemetry ::
      Telemetry
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TelemetryConfigurationUpdate' with the minimum fields required to make a request.
--
-- * 'telemetry' - Configure telemetry to be on or off.
mkTelemetryConfigurationUpdate ::
  -- | 'telemetry'
  Telemetry ->
  TelemetryConfigurationUpdate
mkTelemetryConfigurationUpdate pTelemetry_ =
  TelemetryConfigurationUpdate' {telemetry = pTelemetry_}

-- | Configure telemetry to be on or off.
--
-- /Note:/ Consider using 'telemetry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcuTelemetry :: Lens.Lens' TelemetryConfigurationUpdate Telemetry
tcuTelemetry = Lens.lens (telemetry :: TelemetryConfigurationUpdate -> Telemetry) (\s a -> s {telemetry = a} :: TelemetryConfigurationUpdate)
{-# DEPRECATED tcuTelemetry "Use generic-lens or generic-optics with 'telemetry' instead." #-}

instance Lude.ToJSON TelemetryConfigurationUpdate where
  toJSON TelemetryConfigurationUpdate' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Telemetry" Lude..= telemetry)])

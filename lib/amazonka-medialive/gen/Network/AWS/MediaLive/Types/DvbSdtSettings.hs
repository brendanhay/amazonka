{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbSdtSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbSdtSettings
  ( DvbSdtSettings (..),

    -- * Smart constructor
    mkDvbSdtSettings,

    -- * Lenses
    dssOutputSdt,
    dssRepInterval,
    dssServiceName,
    dssServiceProviderName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.DvbSdtOutputSdt as Types
import qualified Network.AWS.Prelude as Core

-- | DVB Service Description Table (SDT)
--
-- /See:/ 'mkDvbSdtSettings' smart constructor.
data DvbSdtSettings = DvbSdtSettings'
  { -- | Selects method of inserting SDT information into output stream. The sdtFollow setting copies SDT information from input stream to output stream. The sdtFollowIfPresent setting copies SDT information from input stream to output stream if SDT information is present in the input, otherwise it will fall back on the user-defined values. The sdtManual setting means user will enter the SDT information. The sdtNone setting means output stream will not contain SDT information.
    outputSdt :: Core.Maybe Types.DvbSdtOutputSdt,
    -- | The number of milliseconds between instances of this table in the output transport stream.
    repInterval :: Core.Maybe Core.Natural,
    -- | The service name placed in the serviceDescriptor in the Service Description Table. Maximum length is 256 characters.
    serviceName :: Core.Maybe Core.Text,
    -- | The service provider name placed in the serviceDescriptor in the Service Description Table. Maximum length is 256 characters.
    serviceProviderName :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DvbSdtSettings' value with any optional fields omitted.
mkDvbSdtSettings ::
  DvbSdtSettings
mkDvbSdtSettings =
  DvbSdtSettings'
    { outputSdt = Core.Nothing,
      repInterval = Core.Nothing,
      serviceName = Core.Nothing,
      serviceProviderName = Core.Nothing
    }

-- | Selects method of inserting SDT information into output stream. The sdtFollow setting copies SDT information from input stream to output stream. The sdtFollowIfPresent setting copies SDT information from input stream to output stream if SDT information is present in the input, otherwise it will fall back on the user-defined values. The sdtManual setting means user will enter the SDT information. The sdtNone setting means output stream will not contain SDT information.
--
-- /Note:/ Consider using 'outputSdt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssOutputSdt :: Lens.Lens' DvbSdtSettings (Core.Maybe Types.DvbSdtOutputSdt)
dssOutputSdt = Lens.field @"outputSdt"
{-# DEPRECATED dssOutputSdt "Use generic-lens or generic-optics with 'outputSdt' instead." #-}

-- | The number of milliseconds between instances of this table in the output transport stream.
--
-- /Note:/ Consider using 'repInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssRepInterval :: Lens.Lens' DvbSdtSettings (Core.Maybe Core.Natural)
dssRepInterval = Lens.field @"repInterval"
{-# DEPRECATED dssRepInterval "Use generic-lens or generic-optics with 'repInterval' instead." #-}

-- | The service name placed in the serviceDescriptor in the Service Description Table. Maximum length is 256 characters.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssServiceName :: Lens.Lens' DvbSdtSettings (Core.Maybe Core.Text)
dssServiceName = Lens.field @"serviceName"
{-# DEPRECATED dssServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | The service provider name placed in the serviceDescriptor in the Service Description Table. Maximum length is 256 characters.
--
-- /Note:/ Consider using 'serviceProviderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssServiceProviderName :: Lens.Lens' DvbSdtSettings (Core.Maybe Core.Text)
dssServiceProviderName = Lens.field @"serviceProviderName"
{-# DEPRECATED dssServiceProviderName "Use generic-lens or generic-optics with 'serviceProviderName' instead." #-}

instance Core.FromJSON DvbSdtSettings where
  toJSON DvbSdtSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("outputSdt" Core..=) Core.<$> outputSdt,
            ("repInterval" Core..=) Core.<$> repInterval,
            ("serviceName" Core..=) Core.<$> serviceName,
            ("serviceProviderName" Core..=) Core.<$> serviceProviderName
          ]
      )

instance Core.FromJSON DvbSdtSettings where
  parseJSON =
    Core.withObject "DvbSdtSettings" Core.$
      \x ->
        DvbSdtSettings'
          Core.<$> (x Core..:? "outputSdt")
          Core.<*> (x Core..:? "repInterval")
          Core.<*> (x Core..:? "serviceName")
          Core.<*> (x Core..:? "serviceProviderName")

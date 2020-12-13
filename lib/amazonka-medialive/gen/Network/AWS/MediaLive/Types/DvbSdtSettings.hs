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
    dssRepInterval,
    dssServiceProviderName,
    dssOutputSdt,
    dssServiceName,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.DvbSdtOutputSdt
import qualified Network.AWS.Prelude as Lude

-- | DVB Service Description Table (SDT)
--
-- /See:/ 'mkDvbSdtSettings' smart constructor.
data DvbSdtSettings = DvbSdtSettings'
  { -- | The number of milliseconds between instances of this table in the output transport stream.
    repInterval :: Lude.Maybe Lude.Natural,
    -- | The service provider name placed in the serviceDescriptor in the Service Description Table. Maximum length is 256 characters.
    serviceProviderName :: Lude.Maybe Lude.Text,
    -- | Selects method of inserting SDT information into output stream. The sdtFollow setting copies SDT information from input stream to output stream. The sdtFollowIfPresent setting copies SDT information from input stream to output stream if SDT information is present in the input, otherwise it will fall back on the user-defined values. The sdtManual setting means user will enter the SDT information. The sdtNone setting means output stream will not contain SDT information.
    outputSdt :: Lude.Maybe DvbSdtOutputSdt,
    -- | The service name placed in the serviceDescriptor in the Service Description Table. Maximum length is 256 characters.
    serviceName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DvbSdtSettings' with the minimum fields required to make a request.
--
-- * 'repInterval' - The number of milliseconds between instances of this table in the output transport stream.
-- * 'serviceProviderName' - The service provider name placed in the serviceDescriptor in the Service Description Table. Maximum length is 256 characters.
-- * 'outputSdt' - Selects method of inserting SDT information into output stream. The sdtFollow setting copies SDT information from input stream to output stream. The sdtFollowIfPresent setting copies SDT information from input stream to output stream if SDT information is present in the input, otherwise it will fall back on the user-defined values. The sdtManual setting means user will enter the SDT information. The sdtNone setting means output stream will not contain SDT information.
-- * 'serviceName' - The service name placed in the serviceDescriptor in the Service Description Table. Maximum length is 256 characters.
mkDvbSdtSettings ::
  DvbSdtSettings
mkDvbSdtSettings =
  DvbSdtSettings'
    { repInterval = Lude.Nothing,
      serviceProviderName = Lude.Nothing,
      outputSdt = Lude.Nothing,
      serviceName = Lude.Nothing
    }

-- | The number of milliseconds between instances of this table in the output transport stream.
--
-- /Note:/ Consider using 'repInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssRepInterval :: Lens.Lens' DvbSdtSettings (Lude.Maybe Lude.Natural)
dssRepInterval = Lens.lens (repInterval :: DvbSdtSettings -> Lude.Maybe Lude.Natural) (\s a -> s {repInterval = a} :: DvbSdtSettings)
{-# DEPRECATED dssRepInterval "Use generic-lens or generic-optics with 'repInterval' instead." #-}

-- | The service provider name placed in the serviceDescriptor in the Service Description Table. Maximum length is 256 characters.
--
-- /Note:/ Consider using 'serviceProviderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssServiceProviderName :: Lens.Lens' DvbSdtSettings (Lude.Maybe Lude.Text)
dssServiceProviderName = Lens.lens (serviceProviderName :: DvbSdtSettings -> Lude.Maybe Lude.Text) (\s a -> s {serviceProviderName = a} :: DvbSdtSettings)
{-# DEPRECATED dssServiceProviderName "Use generic-lens or generic-optics with 'serviceProviderName' instead." #-}

-- | Selects method of inserting SDT information into output stream. The sdtFollow setting copies SDT information from input stream to output stream. The sdtFollowIfPresent setting copies SDT information from input stream to output stream if SDT information is present in the input, otherwise it will fall back on the user-defined values. The sdtManual setting means user will enter the SDT information. The sdtNone setting means output stream will not contain SDT information.
--
-- /Note:/ Consider using 'outputSdt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssOutputSdt :: Lens.Lens' DvbSdtSettings (Lude.Maybe DvbSdtOutputSdt)
dssOutputSdt = Lens.lens (outputSdt :: DvbSdtSettings -> Lude.Maybe DvbSdtOutputSdt) (\s a -> s {outputSdt = a} :: DvbSdtSettings)
{-# DEPRECATED dssOutputSdt "Use generic-lens or generic-optics with 'outputSdt' instead." #-}

-- | The service name placed in the serviceDescriptor in the Service Description Table. Maximum length is 256 characters.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssServiceName :: Lens.Lens' DvbSdtSettings (Lude.Maybe Lude.Text)
dssServiceName = Lens.lens (serviceName :: DvbSdtSettings -> Lude.Maybe Lude.Text) (\s a -> s {serviceName = a} :: DvbSdtSettings)
{-# DEPRECATED dssServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

instance Lude.FromJSON DvbSdtSettings where
  parseJSON =
    Lude.withObject
      "DvbSdtSettings"
      ( \x ->
          DvbSdtSettings'
            Lude.<$> (x Lude..:? "repInterval")
            Lude.<*> (x Lude..:? "serviceProviderName")
            Lude.<*> (x Lude..:? "outputSdt")
            Lude.<*> (x Lude..:? "serviceName")
      )

instance Lude.ToJSON DvbSdtSettings where
  toJSON DvbSdtSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("repInterval" Lude..=) Lude.<$> repInterval,
            ("serviceProviderName" Lude..=) Lude.<$> serviceProviderName,
            ("outputSdt" Lude..=) Lude.<$> outputSdt,
            ("serviceName" Lude..=) Lude.<$> serviceName
          ]
      )

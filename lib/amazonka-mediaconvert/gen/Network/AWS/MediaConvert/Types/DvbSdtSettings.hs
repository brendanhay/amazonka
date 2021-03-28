{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSdtSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.DvbSdtSettings
  ( DvbSdtSettings (..)
  -- * Smart constructor
  , mkDvbSdtSettings
  -- * Lenses
  , dssOutputSdt
  , dssSdtInterval
  , dssServiceName
  , dssServiceProviderName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.OutputSdt as Types
import qualified Network.AWS.Prelude as Core

-- | Inserts DVB Service Description Table (NIT) at the specified table repetition interval.
--
-- /See:/ 'mkDvbSdtSettings' smart constructor.
data DvbSdtSettings = DvbSdtSettings'
  { outputSdt :: Core.Maybe Types.OutputSdt
    -- ^ Selects method of inserting SDT information into output stream.  "Follow input SDT" copies SDT information from input stream to  output stream. "Follow input SDT if present" copies SDT information from  input stream to output stream if SDT information is present in the input, otherwise it will fall back on the user-defined values. Enter "SDT  Manually" means user will enter the SDT information. "No SDT" means output  stream will not contain SDT information.
  , sdtInterval :: Core.Maybe Core.Natural
    -- ^ The number of milliseconds between instances of this table in the output transport stream.
  , serviceName :: Core.Maybe Core.Text
    -- ^ The service name placed in the service_descriptor in the Service Description Table. Maximum length is 256 characters.
  , serviceProviderName :: Core.Maybe Core.Text
    -- ^ The service provider name placed in the service_descriptor in the Service Description Table. Maximum length is 256 characters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DvbSdtSettings' value with any optional fields omitted.
mkDvbSdtSettings
    :: DvbSdtSettings
mkDvbSdtSettings
  = DvbSdtSettings'{outputSdt = Core.Nothing,
                    sdtInterval = Core.Nothing, serviceName = Core.Nothing,
                    serviceProviderName = Core.Nothing}

-- | Selects method of inserting SDT information into output stream.  "Follow input SDT" copies SDT information from input stream to  output stream. "Follow input SDT if present" copies SDT information from  input stream to output stream if SDT information is present in the input, otherwise it will fall back on the user-defined values. Enter "SDT  Manually" means user will enter the SDT information. "No SDT" means output  stream will not contain SDT information.
--
-- /Note:/ Consider using 'outputSdt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssOutputSdt :: Lens.Lens' DvbSdtSettings (Core.Maybe Types.OutputSdt)
dssOutputSdt = Lens.field @"outputSdt"
{-# INLINEABLE dssOutputSdt #-}
{-# DEPRECATED outputSdt "Use generic-lens or generic-optics with 'outputSdt' instead"  #-}

-- | The number of milliseconds between instances of this table in the output transport stream.
--
-- /Note:/ Consider using 'sdtInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssSdtInterval :: Lens.Lens' DvbSdtSettings (Core.Maybe Core.Natural)
dssSdtInterval = Lens.field @"sdtInterval"
{-# INLINEABLE dssSdtInterval #-}
{-# DEPRECATED sdtInterval "Use generic-lens or generic-optics with 'sdtInterval' instead"  #-}

-- | The service name placed in the service_descriptor in the Service Description Table. Maximum length is 256 characters.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssServiceName :: Lens.Lens' DvbSdtSettings (Core.Maybe Core.Text)
dssServiceName = Lens.field @"serviceName"
{-# INLINEABLE dssServiceName #-}
{-# DEPRECATED serviceName "Use generic-lens or generic-optics with 'serviceName' instead"  #-}

-- | The service provider name placed in the service_descriptor in the Service Description Table. Maximum length is 256 characters.
--
-- /Note:/ Consider using 'serviceProviderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssServiceProviderName :: Lens.Lens' DvbSdtSettings (Core.Maybe Core.Text)
dssServiceProviderName = Lens.field @"serviceProviderName"
{-# INLINEABLE dssServiceProviderName #-}
{-# DEPRECATED serviceProviderName "Use generic-lens or generic-optics with 'serviceProviderName' instead"  #-}

instance Core.FromJSON DvbSdtSettings where
        toJSON DvbSdtSettings{..}
          = Core.object
              (Core.catMaybes
                 [("outputSdt" Core..=) Core.<$> outputSdt,
                  ("sdtInterval" Core..=) Core.<$> sdtInterval,
                  ("serviceName" Core..=) Core.<$> serviceName,
                  ("serviceProviderName" Core..=) Core.<$> serviceProviderName])

instance Core.FromJSON DvbSdtSettings where
        parseJSON
          = Core.withObject "DvbSdtSettings" Core.$
              \ x ->
                DvbSdtSettings' Core.<$>
                  (x Core..:? "outputSdt") Core.<*> x Core..:? "sdtInterval" Core.<*>
                    x Core..:? "serviceName"
                    Core.<*> x Core..:? "serviceProviderName"

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.ConferenceProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.ConferenceProvider
  ( ConferenceProvider (..)
  -- * Smart constructor
  , mkConferenceProvider
  -- * Lenses
  , cpArn
  , cpIPDialIn
  , cpMeetingSetting
  , cpName
  , cpPSTNDialIn
  , cpType
  ) where

import qualified Network.AWS.AlexaBusiness.Types.Arn as Types
import qualified Network.AWS.AlexaBusiness.Types.ConferenceProviderType as Types
import qualified Network.AWS.AlexaBusiness.Types.IPDialIn as Types
import qualified Network.AWS.AlexaBusiness.Types.MeetingSetting as Types
import qualified Network.AWS.AlexaBusiness.Types.Name as Types
import qualified Network.AWS.AlexaBusiness.Types.PSTNDialIn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An entity that provides a conferencing solution. Alexa for Business acts as the voice interface and mediator that connects users to their preferred conference provider. Examples of conference providers include Amazon Chime, Zoom, Cisco, and Polycom. 
--
-- /See:/ 'mkConferenceProvider' smart constructor.
data ConferenceProvider = ConferenceProvider'
  { arn :: Core.Maybe Types.Arn
    -- ^ The ARN of the newly created conference provider.
  , iPDialIn :: Core.Maybe Types.IPDialIn
    -- ^ The IP endpoint and protocol for calling.
  , meetingSetting :: Core.Maybe Types.MeetingSetting
    -- ^ The meeting settings for the conference provider.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the conference provider.
  , pSTNDialIn :: Core.Maybe Types.PSTNDialIn
    -- ^ The information for PSTN conferencing.
  , type' :: Core.Maybe Types.ConferenceProviderType
    -- ^ The type of conference providers.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConferenceProvider' value with any optional fields omitted.
mkConferenceProvider
    :: ConferenceProvider
mkConferenceProvider
  = ConferenceProvider'{arn = Core.Nothing, iPDialIn = Core.Nothing,
                        meetingSetting = Core.Nothing, name = Core.Nothing,
                        pSTNDialIn = Core.Nothing, type' = Core.Nothing}

-- | The ARN of the newly created conference provider.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpArn :: Lens.Lens' ConferenceProvider (Core.Maybe Types.Arn)
cpArn = Lens.field @"arn"
{-# INLINEABLE cpArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The IP endpoint and protocol for calling.
--
-- /Note:/ Consider using 'iPDialIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpIPDialIn :: Lens.Lens' ConferenceProvider (Core.Maybe Types.IPDialIn)
cpIPDialIn = Lens.field @"iPDialIn"
{-# INLINEABLE cpIPDialIn #-}
{-# DEPRECATED iPDialIn "Use generic-lens or generic-optics with 'iPDialIn' instead"  #-}

-- | The meeting settings for the conference provider.
--
-- /Note:/ Consider using 'meetingSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpMeetingSetting :: Lens.Lens' ConferenceProvider (Core.Maybe Types.MeetingSetting)
cpMeetingSetting = Lens.field @"meetingSetting"
{-# INLINEABLE cpMeetingSetting #-}
{-# DEPRECATED meetingSetting "Use generic-lens or generic-optics with 'meetingSetting' instead"  #-}

-- | The name of the conference provider.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' ConferenceProvider (Core.Maybe Types.Name)
cpName = Lens.field @"name"
{-# INLINEABLE cpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The information for PSTN conferencing.
--
-- /Note:/ Consider using 'pSTNDialIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPSTNDialIn :: Lens.Lens' ConferenceProvider (Core.Maybe Types.PSTNDialIn)
cpPSTNDialIn = Lens.field @"pSTNDialIn"
{-# INLINEABLE cpPSTNDialIn #-}
{-# DEPRECATED pSTNDialIn "Use generic-lens or generic-optics with 'pSTNDialIn' instead"  #-}

-- | The type of conference providers.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpType :: Lens.Lens' ConferenceProvider (Core.Maybe Types.ConferenceProviderType)
cpType = Lens.field @"type'"
{-# INLINEABLE cpType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON ConferenceProvider where
        parseJSON
          = Core.withObject "ConferenceProvider" Core.$
              \ x ->
                ConferenceProvider' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "IPDialIn" Core.<*>
                    x Core..:? "MeetingSetting"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "PSTNDialIn"
                    Core.<*> x Core..:? "Type"

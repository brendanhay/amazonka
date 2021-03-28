{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SipAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.SipAddress
  ( SipAddress (..)
  -- * Smart constructor
  , mkSipAddress
  -- * Lenses
  , saUri
  , saType
  ) where

import qualified Network.AWS.AlexaBusiness.Types.SipType as Types
import qualified Network.AWS.AlexaBusiness.Types.Uri as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The SIP address for the contact containing the URI and SIP address type.
--
-- /See:/ 'mkSipAddress' smart constructor.
data SipAddress = SipAddress'
  { uri :: Types.Uri
    -- ^ The URI for the SIP address.
  , type' :: Types.SipType
    -- ^ The type of the SIP address.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SipAddress' value with any optional fields omitted.
mkSipAddress
    :: Types.Uri -- ^ 'uri'
    -> Types.SipType -- ^ 'type\''
    -> SipAddress
mkSipAddress uri type' = SipAddress'{uri, type'}

-- | The URI for the SIP address.
--
-- /Note:/ Consider using 'uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saUri :: Lens.Lens' SipAddress Types.Uri
saUri = Lens.field @"uri"
{-# INLINEABLE saUri #-}
{-# DEPRECATED uri "Use generic-lens or generic-optics with 'uri' instead"  #-}

-- | The type of the SIP address.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saType :: Lens.Lens' SipAddress Types.SipType
saType = Lens.field @"type'"
{-# INLINEABLE saType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON SipAddress where
        toJSON SipAddress{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Uri" Core..= uri), Core.Just ("Type" Core..= type')])

instance Core.FromJSON SipAddress where
        parseJSON
          = Core.withObject "SipAddress" Core.$
              \ x ->
                SipAddress' Core.<$> (x Core..: "Uri") Core.<*> x Core..: "Type"

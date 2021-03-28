{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.RequestPaymentConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.RequestPaymentConfiguration
  ( RequestPaymentConfiguration (..)
  -- * Smart constructor
  , mkRequestPaymentConfiguration
  -- * Lenses
  , rpcPayer
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Payer as Types

-- | Container for Payer.
--
-- /See:/ 'mkRequestPaymentConfiguration' smart constructor.
newtype RequestPaymentConfiguration = RequestPaymentConfiguration'
  { payer :: Types.Payer
    -- ^ Specifies who pays for the download and request fees.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RequestPaymentConfiguration' value with any optional fields omitted.
mkRequestPaymentConfiguration
    :: Types.Payer -- ^ 'payer'
    -> RequestPaymentConfiguration
mkRequestPaymentConfiguration payer
  = RequestPaymentConfiguration'{payer}

-- | Specifies who pays for the download and request fees.
--
-- /Note:/ Consider using 'payer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpcPayer :: Lens.Lens' RequestPaymentConfiguration Types.Payer
rpcPayer = Lens.field @"payer"
{-# INLINEABLE rpcPayer #-}
{-# DEPRECATED payer "Use generic-lens or generic-optics with 'payer' instead"  #-}

instance Core.ToXML RequestPaymentConfiguration where
        toXML RequestPaymentConfiguration{..}
          = Core.toXMLElement "Payer" payer

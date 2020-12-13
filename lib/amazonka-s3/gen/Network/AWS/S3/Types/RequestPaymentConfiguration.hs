{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.RequestPaymentConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.RequestPaymentConfiguration
  ( RequestPaymentConfiguration (..),

    -- * Smart constructor
    mkRequestPaymentConfiguration,

    -- * Lenses
    rpcPayer,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Payer

-- | Container for Payer.
--
-- /See:/ 'mkRequestPaymentConfiguration' smart constructor.
newtype RequestPaymentConfiguration = RequestPaymentConfiguration'
  { -- | Specifies who pays for the download and request fees.
    payer :: Payer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RequestPaymentConfiguration' with the minimum fields required to make a request.
--
-- * 'payer' - Specifies who pays for the download and request fees.
mkRequestPaymentConfiguration ::
  -- | 'payer'
  Payer ->
  RequestPaymentConfiguration
mkRequestPaymentConfiguration pPayer_ =
  RequestPaymentConfiguration' {payer = pPayer_}

-- | Specifies who pays for the download and request fees.
--
-- /Note:/ Consider using 'payer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpcPayer :: Lens.Lens' RequestPaymentConfiguration Payer
rpcPayer = Lens.lens (payer :: RequestPaymentConfiguration -> Payer) (\s a -> s {payer = a} :: RequestPaymentConfiguration)
{-# DEPRECATED rpcPayer "Use generic-lens or generic-optics with 'payer' instead." #-}

instance Lude.ToXML RequestPaymentConfiguration where
  toXML RequestPaymentConfiguration' {..} =
    Lude.mconcat ["Payer" Lude.@= payer]

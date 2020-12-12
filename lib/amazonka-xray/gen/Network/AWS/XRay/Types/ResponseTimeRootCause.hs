{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ResponseTimeRootCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ResponseTimeRootCause
  ( ResponseTimeRootCause (..),

    -- * Smart constructor
    mkResponseTimeRootCause,

    -- * Lenses
    rtrcClientImpacting,
    rtrcServices,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.ResponseTimeRootCauseService

-- | The root cause information for a response time warning.
--
-- /See:/ 'mkResponseTimeRootCause' smart constructor.
data ResponseTimeRootCause = ResponseTimeRootCause'
  { clientImpacting ::
      Lude.Maybe Lude.Bool,
    services ::
      Lude.Maybe [ResponseTimeRootCauseService]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResponseTimeRootCause' with the minimum fields required to make a request.
--
-- * 'clientImpacting' - A flag that denotes that the root cause impacts the trace client.
-- * 'services' - A list of corresponding services. A service identifies a segment and contains a name, account ID, type, and inferred flag.
mkResponseTimeRootCause ::
  ResponseTimeRootCause
mkResponseTimeRootCause =
  ResponseTimeRootCause'
    { clientImpacting = Lude.Nothing,
      services = Lude.Nothing
    }

-- | A flag that denotes that the root cause impacts the trace client.
--
-- /Note:/ Consider using 'clientImpacting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrcClientImpacting :: Lens.Lens' ResponseTimeRootCause (Lude.Maybe Lude.Bool)
rtrcClientImpacting = Lens.lens (clientImpacting :: ResponseTimeRootCause -> Lude.Maybe Lude.Bool) (\s a -> s {clientImpacting = a} :: ResponseTimeRootCause)
{-# DEPRECATED rtrcClientImpacting "Use generic-lens or generic-optics with 'clientImpacting' instead." #-}

-- | A list of corresponding services. A service identifies a segment and contains a name, account ID, type, and inferred flag.
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrcServices :: Lens.Lens' ResponseTimeRootCause (Lude.Maybe [ResponseTimeRootCauseService])
rtrcServices = Lens.lens (services :: ResponseTimeRootCause -> Lude.Maybe [ResponseTimeRootCauseService]) (\s a -> s {services = a} :: ResponseTimeRootCause)
{-# DEPRECATED rtrcServices "Use generic-lens or generic-optics with 'services' instead." #-}

instance Lude.FromJSON ResponseTimeRootCause where
  parseJSON =
    Lude.withObject
      "ResponseTimeRootCause"
      ( \x ->
          ResponseTimeRootCause'
            Lude.<$> (x Lude..:? "ClientImpacting")
            Lude.<*> (x Lude..:? "Services" Lude..!= Lude.mempty)
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.FaultRootCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.FaultRootCause
  ( FaultRootCause (..),

    -- * Smart constructor
    mkFaultRootCause,

    -- * Lenses
    frcClientImpacting,
    frcServices,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.FaultRootCauseService

-- | The root cause information for a trace summary fault.
--
-- /See:/ 'mkFaultRootCause' smart constructor.
data FaultRootCause = FaultRootCause'
  { -- | A flag that denotes that the root cause impacts the trace client.
    clientImpacting :: Lude.Maybe Lude.Bool,
    -- | A list of corresponding services. A service identifies a segment and it contains a name, account ID, type, and inferred flag.
    services :: Lude.Maybe [FaultRootCauseService]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FaultRootCause' with the minimum fields required to make a request.
--
-- * 'clientImpacting' - A flag that denotes that the root cause impacts the trace client.
-- * 'services' - A list of corresponding services. A service identifies a segment and it contains a name, account ID, type, and inferred flag.
mkFaultRootCause ::
  FaultRootCause
mkFaultRootCause =
  FaultRootCause'
    { clientImpacting = Lude.Nothing,
      services = Lude.Nothing
    }

-- | A flag that denotes that the root cause impacts the trace client.
--
-- /Note:/ Consider using 'clientImpacting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frcClientImpacting :: Lens.Lens' FaultRootCause (Lude.Maybe Lude.Bool)
frcClientImpacting = Lens.lens (clientImpacting :: FaultRootCause -> Lude.Maybe Lude.Bool) (\s a -> s {clientImpacting = a} :: FaultRootCause)
{-# DEPRECATED frcClientImpacting "Use generic-lens or generic-optics with 'clientImpacting' instead." #-}

-- | A list of corresponding services. A service identifies a segment and it contains a name, account ID, type, and inferred flag.
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frcServices :: Lens.Lens' FaultRootCause (Lude.Maybe [FaultRootCauseService])
frcServices = Lens.lens (services :: FaultRootCause -> Lude.Maybe [FaultRootCauseService]) (\s a -> s {services = a} :: FaultRootCause)
{-# DEPRECATED frcServices "Use generic-lens or generic-optics with 'services' instead." #-}

instance Lude.FromJSON FaultRootCause where
  parseJSON =
    Lude.withObject
      "FaultRootCause"
      ( \x ->
          FaultRootCause'
            Lude.<$> (x Lude..:? "ClientImpacting")
            Lude.<*> (x Lude..:? "Services" Lude..!= Lude.mempty)
      )

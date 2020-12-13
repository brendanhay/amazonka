{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ErrorRootCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ErrorRootCause
  ( ErrorRootCause (..),

    -- * Smart constructor
    mkErrorRootCause,

    -- * Lenses
    ercClientImpacting,
    ercServices,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.ErrorRootCauseService

-- | The root cause of a trace summary error.
--
-- /See:/ 'mkErrorRootCause' smart constructor.
data ErrorRootCause = ErrorRootCause'
  { -- | A flag that denotes that the root cause impacts the trace client.
    clientImpacting :: Lude.Maybe Lude.Bool,
    -- | A list of services corresponding to an error. A service identifies a segment and it contains a name, account ID, type, and inferred flag.
    services :: Lude.Maybe [ErrorRootCauseService]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ErrorRootCause' with the minimum fields required to make a request.
--
-- * 'clientImpacting' - A flag that denotes that the root cause impacts the trace client.
-- * 'services' - A list of services corresponding to an error. A service identifies a segment and it contains a name, account ID, type, and inferred flag.
mkErrorRootCause ::
  ErrorRootCause
mkErrorRootCause =
  ErrorRootCause'
    { clientImpacting = Lude.Nothing,
      services = Lude.Nothing
    }

-- | A flag that denotes that the root cause impacts the trace client.
--
-- /Note:/ Consider using 'clientImpacting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ercClientImpacting :: Lens.Lens' ErrorRootCause (Lude.Maybe Lude.Bool)
ercClientImpacting = Lens.lens (clientImpacting :: ErrorRootCause -> Lude.Maybe Lude.Bool) (\s a -> s {clientImpacting = a} :: ErrorRootCause)
{-# DEPRECATED ercClientImpacting "Use generic-lens or generic-optics with 'clientImpacting' instead." #-}

-- | A list of services corresponding to an error. A service identifies a segment and it contains a name, account ID, type, and inferred flag.
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ercServices :: Lens.Lens' ErrorRootCause (Lude.Maybe [ErrorRootCauseService])
ercServices = Lens.lens (services :: ErrorRootCause -> Lude.Maybe [ErrorRootCauseService]) (\s a -> s {services = a} :: ErrorRootCause)
{-# DEPRECATED ercServices "Use generic-lens or generic-optics with 'services' instead." #-}

instance Lude.FromJSON ErrorRootCause where
  parseJSON =
    Lude.withObject
      "ErrorRootCause"
      ( \x ->
          ErrorRootCause'
            Lude.<$> (x Lude..:? "ClientImpacting")
            Lude.<*> (x Lude..:? "Services" Lude..!= Lude.mempty)
      )

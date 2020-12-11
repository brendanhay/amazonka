-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.TraceUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.TraceUser
  ( TraceUser (..),

    -- * Smart constructor
    mkTraceUser,

    -- * Lenses
    tuServiceIds,
    tuUserName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.ServiceId

-- | Information about a user recorded in segment documents.
--
-- /See:/ 'mkTraceUser' smart constructor.
data TraceUser = TraceUser'
  { serviceIds :: Lude.Maybe [ServiceId],
    userName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TraceUser' with the minimum fields required to make a request.
--
-- * 'serviceIds' - Services that the user's request hit.
-- * 'userName' - The user's name.
mkTraceUser ::
  TraceUser
mkTraceUser =
  TraceUser' {serviceIds = Lude.Nothing, userName = Lude.Nothing}

-- | Services that the user's request hit.
--
-- /Note:/ Consider using 'serviceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tuServiceIds :: Lens.Lens' TraceUser (Lude.Maybe [ServiceId])
tuServiceIds = Lens.lens (serviceIds :: TraceUser -> Lude.Maybe [ServiceId]) (\s a -> s {serviceIds = a} :: TraceUser)
{-# DEPRECATED tuServiceIds "Use generic-lens or generic-optics with 'serviceIds' instead." #-}

-- | The user's name.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tuUserName :: Lens.Lens' TraceUser (Lude.Maybe Lude.Text)
tuUserName = Lens.lens (userName :: TraceUser -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: TraceUser)
{-# DEPRECATED tuUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Lude.FromJSON TraceUser where
  parseJSON =
    Lude.withObject
      "TraceUser"
      ( \x ->
          TraceUser'
            Lude.<$> (x Lude..:? "ServiceIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "UserName")
      )

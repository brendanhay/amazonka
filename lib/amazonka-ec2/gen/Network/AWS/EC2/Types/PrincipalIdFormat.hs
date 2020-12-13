{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrincipalIdFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrincipalIdFormat
  ( PrincipalIdFormat (..),

    -- * Smart constructor
    mkPrincipalIdFormat,

    -- * Lenses
    pifARN,
    pifStatuses,
  )
where

import Network.AWS.EC2.Types.IdFormat
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | PrincipalIdFormat description
--
-- /See:/ 'mkPrincipalIdFormat' smart constructor.
data PrincipalIdFormat = PrincipalIdFormat'
  { -- | PrincipalIdFormatARN description
    arn :: Lude.Maybe Lude.Text,
    -- | PrincipalIdFormatStatuses description
    statuses :: Lude.Maybe [IdFormat]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PrincipalIdFormat' with the minimum fields required to make a request.
--
-- * 'arn' - PrincipalIdFormatARN description
-- * 'statuses' - PrincipalIdFormatStatuses description
mkPrincipalIdFormat ::
  PrincipalIdFormat
mkPrincipalIdFormat =
  PrincipalIdFormat' {arn = Lude.Nothing, statuses = Lude.Nothing}

-- | PrincipalIdFormatARN description
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pifARN :: Lens.Lens' PrincipalIdFormat (Lude.Maybe Lude.Text)
pifARN = Lens.lens (arn :: PrincipalIdFormat -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: PrincipalIdFormat)
{-# DEPRECATED pifARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | PrincipalIdFormatStatuses description
--
-- /Note:/ Consider using 'statuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pifStatuses :: Lens.Lens' PrincipalIdFormat (Lude.Maybe [IdFormat])
pifStatuses = Lens.lens (statuses :: PrincipalIdFormat -> Lude.Maybe [IdFormat]) (\s a -> s {statuses = a} :: PrincipalIdFormat)
{-# DEPRECATED pifStatuses "Use generic-lens or generic-optics with 'statuses' instead." #-}

instance Lude.FromXML PrincipalIdFormat where
  parseXML x =
    PrincipalIdFormat'
      Lude.<$> (x Lude..@? "arn")
      Lude.<*> ( x Lude..@? "statusSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )

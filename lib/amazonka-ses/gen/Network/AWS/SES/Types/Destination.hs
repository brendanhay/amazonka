{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.Destination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.Destination
  ( Destination (..),

    -- * Smart constructor
    mkDestination,

    -- * Lenses
    dBCCAddresses,
    dCCAddresses,
    dToAddresses,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the destination of the message, consisting of To:, CC:, and BCC: fields.
--
-- /See:/ 'mkDestination' smart constructor.
data Destination = Destination'
  { bCCAddresses ::
      Lude.Maybe [Lude.Text],
    cCAddresses :: Lude.Maybe [Lude.Text],
    toAddresses :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Destination' with the minimum fields required to make a request.
--
-- * 'bCCAddresses' - The recipients to place on the BCC: line of the message.
-- * 'cCAddresses' - The recipients to place on the CC: line of the message.
-- * 'toAddresses' - The recipients to place on the To: line of the message.
mkDestination ::
  Destination
mkDestination =
  Destination'
    { bCCAddresses = Lude.Nothing,
      cCAddresses = Lude.Nothing,
      toAddresses = Lude.Nothing
    }

-- | The recipients to place on the BCC: line of the message.
--
-- /Note:/ Consider using 'bCCAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dBCCAddresses :: Lens.Lens' Destination (Lude.Maybe [Lude.Text])
dBCCAddresses = Lens.lens (bCCAddresses :: Destination -> Lude.Maybe [Lude.Text]) (\s a -> s {bCCAddresses = a} :: Destination)
{-# DEPRECATED dBCCAddresses "Use generic-lens or generic-optics with 'bCCAddresses' instead." #-}

-- | The recipients to place on the CC: line of the message.
--
-- /Note:/ Consider using 'cCAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCCAddresses :: Lens.Lens' Destination (Lude.Maybe [Lude.Text])
dCCAddresses = Lens.lens (cCAddresses :: Destination -> Lude.Maybe [Lude.Text]) (\s a -> s {cCAddresses = a} :: Destination)
{-# DEPRECATED dCCAddresses "Use generic-lens or generic-optics with 'cCAddresses' instead." #-}

-- | The recipients to place on the To: line of the message.
--
-- /Note:/ Consider using 'toAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dToAddresses :: Lens.Lens' Destination (Lude.Maybe [Lude.Text])
dToAddresses = Lens.lens (toAddresses :: Destination -> Lude.Maybe [Lude.Text]) (\s a -> s {toAddresses = a} :: Destination)
{-# DEPRECATED dToAddresses "Use generic-lens or generic-optics with 'toAddresses' instead." #-}

instance Lude.ToQuery Destination where
  toQuery Destination' {..} =
    Lude.mconcat
      [ "BccAddresses"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> bCCAddresses),
        "CcAddresses"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> cCAddresses),
        "ToAddresses"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> toAddresses)
      ]

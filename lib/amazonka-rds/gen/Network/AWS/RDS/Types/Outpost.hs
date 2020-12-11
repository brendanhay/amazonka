-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Outpost
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Outpost
  ( Outpost (..),

    -- * Smart constructor
    mkOutpost,

    -- * Lenses
    oARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A data type that represents an Outpost.
--
-- For more information about RDS on Outposts, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on AWS Outposts> in the /Amazon RDS User Guide./
--
-- /See:/ 'mkOutpost' smart constructor.
newtype Outpost = Outpost' {arn :: Lude.Maybe Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Outpost' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the Outpost.
mkOutpost ::
  Outpost
mkOutpost = Outpost' {arn = Lude.Nothing}

-- | The Amazon Resource Name (ARN) of the Outpost.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oARN :: Lens.Lens' Outpost (Lude.Maybe Lude.Text)
oARN = Lens.lens (arn :: Outpost -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Outpost)
{-# DEPRECATED oARN "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.FromXML Outpost where
  parseXML x = Outpost' Lude.<$> (x Lude..@? "Arn")

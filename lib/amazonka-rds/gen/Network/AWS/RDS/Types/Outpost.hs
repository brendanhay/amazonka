{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    oArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types

-- | A data type that represents an Outpost.
--
-- For more information about RDS on Outposts, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on AWS Outposts> in the /Amazon RDS User Guide./
--
-- /See:/ 'mkOutpost' smart constructor.
newtype Outpost = Outpost'
  { -- | The Amazon Resource Name (ARN) of the Outpost.
    arn :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Outpost' value with any optional fields omitted.
mkOutpost ::
  Outpost
mkOutpost = Outpost' {arn = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the Outpost.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oArn :: Lens.Lens' Outpost (Core.Maybe Types.String)
oArn = Lens.field @"arn"
{-# DEPRECATED oArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Core.FromXML Outpost where
  parseXML x = Outpost' Core.<$> (x Core..@? "Arn")

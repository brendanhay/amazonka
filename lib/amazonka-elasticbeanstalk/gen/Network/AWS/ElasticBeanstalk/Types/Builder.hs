{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Builder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.Builder
  ( Builder (..),

    -- * Smart constructor
    mkBuilder,

    -- * Lenses
    bARN,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.ARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The builder used to build the custom platform.
--
-- /See:/ 'mkBuilder' smart constructor.
newtype Builder = Builder'
  { -- | The ARN of the builder.
    arn :: Core.Maybe Types.ARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Builder' value with any optional fields omitted.
mkBuilder ::
  Builder
mkBuilder = Builder' {arn = Core.Nothing}

-- | The ARN of the builder.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bARN :: Lens.Lens' Builder (Core.Maybe Types.ARN)
bARN = Lens.field @"arn"
{-# DEPRECATED bARN "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Core.FromXML Builder where
  parseXML x = Builder' Core.<$> (x Core..@? "ARN")

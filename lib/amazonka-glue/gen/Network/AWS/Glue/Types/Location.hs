{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Location
  ( Location (..),

    -- * Smart constructor
    mkLocation,

    -- * Lenses
    lDynamoDB,
    lJdbc,
    lS3,
  )
where

import qualified Network.AWS.Glue.Types.CodeGenNodeArg as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The location of resources.
--
-- /See:/ 'mkLocation' smart constructor.
data Location = Location'
  { -- | An Amazon DynamoDB table location.
    dynamoDB :: Core.Maybe [Types.CodeGenNodeArg],
    -- | A JDBC location.
    jdbc :: Core.Maybe [Types.CodeGenNodeArg],
    -- | An Amazon Simple Storage Service (Amazon S3) location.
    s3 :: Core.Maybe [Types.CodeGenNodeArg]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Location' value with any optional fields omitted.
mkLocation ::
  Location
mkLocation =
  Location'
    { dynamoDB = Core.Nothing,
      jdbc = Core.Nothing,
      s3 = Core.Nothing
    }

-- | An Amazon DynamoDB table location.
--
-- /Note:/ Consider using 'dynamoDB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lDynamoDB :: Lens.Lens' Location (Core.Maybe [Types.CodeGenNodeArg])
lDynamoDB = Lens.field @"dynamoDB"
{-# DEPRECATED lDynamoDB "Use generic-lens or generic-optics with 'dynamoDB' instead." #-}

-- | A JDBC location.
--
-- /Note:/ Consider using 'jdbc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lJdbc :: Lens.Lens' Location (Core.Maybe [Types.CodeGenNodeArg])
lJdbc = Lens.field @"jdbc"
{-# DEPRECATED lJdbc "Use generic-lens or generic-optics with 'jdbc' instead." #-}

-- | An Amazon Simple Storage Service (Amazon S3) location.
--
-- /Note:/ Consider using 's3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lS3 :: Lens.Lens' Location (Core.Maybe [Types.CodeGenNodeArg])
lS3 = Lens.field @"s3"
{-# DEPRECATED lS3 "Use generic-lens or generic-optics with 's3' instead." #-}

instance Core.FromJSON Location where
  toJSON Location {..} =
    Core.object
      ( Core.catMaybes
          [ ("DynamoDB" Core..=) Core.<$> dynamoDB,
            ("Jdbc" Core..=) Core.<$> jdbc,
            ("S3" Core..=) Core.<$> s3
          ]
      )

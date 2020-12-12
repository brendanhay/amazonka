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

import Network.AWS.Glue.Types.CodeGenNodeArg
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The location of resources.
--
-- /See:/ 'mkLocation' smart constructor.
data Location = Location'
  { dynamoDB :: Lude.Maybe [CodeGenNodeArg],
    jdbc :: Lude.Maybe [CodeGenNodeArg],
    s3 :: Lude.Maybe [CodeGenNodeArg]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Location' with the minimum fields required to make a request.
--
-- * 'dynamoDB' - An Amazon DynamoDB table location.
-- * 'jdbc' - A JDBC location.
-- * 's3' - An Amazon Simple Storage Service (Amazon S3) location.
mkLocation ::
  Location
mkLocation =
  Location'
    { dynamoDB = Lude.Nothing,
      jdbc = Lude.Nothing,
      s3 = Lude.Nothing
    }

-- | An Amazon DynamoDB table location.
--
-- /Note:/ Consider using 'dynamoDB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lDynamoDB :: Lens.Lens' Location (Lude.Maybe [CodeGenNodeArg])
lDynamoDB = Lens.lens (dynamoDB :: Location -> Lude.Maybe [CodeGenNodeArg]) (\s a -> s {dynamoDB = a} :: Location)
{-# DEPRECATED lDynamoDB "Use generic-lens or generic-optics with 'dynamoDB' instead." #-}

-- | A JDBC location.
--
-- /Note:/ Consider using 'jdbc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lJdbc :: Lens.Lens' Location (Lude.Maybe [CodeGenNodeArg])
lJdbc = Lens.lens (jdbc :: Location -> Lude.Maybe [CodeGenNodeArg]) (\s a -> s {jdbc = a} :: Location)
{-# DEPRECATED lJdbc "Use generic-lens or generic-optics with 'jdbc' instead." #-}

-- | An Amazon Simple Storage Service (Amazon S3) location.
--
-- /Note:/ Consider using 's3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lS3 :: Lens.Lens' Location (Lude.Maybe [CodeGenNodeArg])
lS3 = Lens.lens (s3 :: Location -> Lude.Maybe [CodeGenNodeArg]) (\s a -> s {s3 = a} :: Location)
{-# DEPRECATED lS3 "Use generic-lens or generic-optics with 's3' instead." #-}

instance Lude.ToJSON Location where
  toJSON Location' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DynamoDB" Lude..=) Lude.<$> dynamoDB,
            ("Jdbc" Lude..=) Lude.<$> jdbc,
            ("S3" Lude..=) Lude.<$> s3
          ]
      )

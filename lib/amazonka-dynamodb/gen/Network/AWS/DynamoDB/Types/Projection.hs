-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Projection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Projection
  ( Projection (..),

    -- * Smart constructor
    mkProjection,

    -- * Lenses
    pProjectionType,
    pNonKeyAttributes,
  )
where

import Network.AWS.DynamoDB.Types.ProjectionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents attributes that are copied (projected) from the table into an index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
--
-- /See:/ 'mkProjection' smart constructor.
data Projection = Projection'
  { projectionType ::
      Lude.Maybe ProjectionType,
    nonKeyAttributes :: Lude.Maybe (Lude.NonEmpty Lude.Text)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Projection' with the minimum fields required to make a request.
--
-- * 'nonKeyAttributes' - Represents the non-key attribute names which will be projected into the index.
--
-- For local secondary indexes, the total count of @NonKeyAttributes@ summed across all of the local secondary indexes, must not exceed 20. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.
-- * 'projectionType' - The set of attributes that are projected into the index:
--
--
--     * @KEYS_ONLY@ - Only the index and primary keys are projected into the index.
--
--
--     * @INCLUDE@ - In addition to the attributes described in @KEYS_ONLY@ , the secondary index will include other non-key attributes that you specify.
--
--
--     * @ALL@ - All of the table attributes are projected into the index.
mkProjection ::
  Projection
mkProjection =
  Projection'
    { projectionType = Lude.Nothing,
      nonKeyAttributes = Lude.Nothing
    }

-- | The set of attributes that are projected into the index:
--
--
--     * @KEYS_ONLY@ - Only the index and primary keys are projected into the index.
--
--
--     * @INCLUDE@ - In addition to the attributes described in @KEYS_ONLY@ , the secondary index will include other non-key attributes that you specify.
--
--
--     * @ALL@ - All of the table attributes are projected into the index.
--
--
--
-- /Note:/ Consider using 'projectionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pProjectionType :: Lens.Lens' Projection (Lude.Maybe ProjectionType)
pProjectionType = Lens.lens (projectionType :: Projection -> Lude.Maybe ProjectionType) (\s a -> s {projectionType = a} :: Projection)
{-# DEPRECATED pProjectionType "Use generic-lens or generic-optics with 'projectionType' instead." #-}

-- | Represents the non-key attribute names which will be projected into the index.
--
-- For local secondary indexes, the total count of @NonKeyAttributes@ summed across all of the local secondary indexes, must not exceed 20. If you project the same attribute into two different indexes, this counts as two distinct attributes when determining the total.
--
-- /Note:/ Consider using 'nonKeyAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pNonKeyAttributes :: Lens.Lens' Projection (Lude.Maybe (Lude.NonEmpty Lude.Text))
pNonKeyAttributes = Lens.lens (nonKeyAttributes :: Projection -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {nonKeyAttributes = a} :: Projection)
{-# DEPRECATED pNonKeyAttributes "Use generic-lens or generic-optics with 'nonKeyAttributes' instead." #-}

instance Lude.FromJSON Projection where
  parseJSON =
    Lude.withObject
      "Projection"
      ( \x ->
          Projection'
            Lude.<$> (x Lude..:? "ProjectionType")
            Lude.<*> (x Lude..:? "NonKeyAttributes")
      )

instance Lude.ToJSON Projection where
  toJSON Projection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProjectionType" Lude..=) Lude.<$> projectionType,
            ("NonKeyAttributes" Lude..=) Lude.<$> nonKeyAttributes
          ]
      )

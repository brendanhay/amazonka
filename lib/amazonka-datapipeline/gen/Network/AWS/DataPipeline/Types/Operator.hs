{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.Operator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.Operator
  ( Operator (..),

    -- * Smart constructor
    mkOperator,

    -- * Lenses
    oValues,
    oType,
  )
where

import Network.AWS.DataPipeline.Types.OperatorType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains a logical operation for comparing the value of a field with a specified value.
--
-- /See:/ 'mkOperator' smart constructor.
data Operator = Operator'
  { values :: Lude.Maybe [Lude.Text],
    type' :: Lude.Maybe OperatorType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Operator' with the minimum fields required to make a request.
--
-- * 'type'' - The logical operation to be performed: equal (@EQ@ ), equal reference (@REF_EQ@ ), less than or equal (@LE@ ), greater than or equal (@GE@ ), or between (@BETWEEN@ ). Equal reference (@REF_EQ@ ) can be used only with reference fields. The other comparison types can be used only with String fields. The comparison types you can use apply only to certain object fields, as detailed below.
--
-- The comparison operators EQ and REF_EQ act on the following fields:
--
--     * name
--
--     * @sphere
--
--     * parent
--
--     * @componentParent
--
--     * @instanceParent
--
--     * @status
--
--     * @scheduledStartTime
--
--     * @scheduledEndTime
--
--     * @actualStartTime
--
--     * @actualEndTime
--
-- The comparison operators @GE@ , @LE@ , and @BETWEEN@ act on the following fields:
--
--     * @scheduledStartTime
--
--     * @scheduledEndTime
--
--     * @actualStartTime
--
--     * @actualEndTime
--
-- Note that fields beginning with the at sign (@) are read-only and set by the web service. When you name fields, you should choose names containing only alpha-numeric values, as symbols may be reserved by AWS Data Pipeline. User-defined fields that you add to a pipeline should prefix their name with the string "my".
-- * 'values' - The value that the actual field value will be compared with.
mkOperator ::
  Operator
mkOperator = Operator' {values = Lude.Nothing, type' = Lude.Nothing}

-- | The value that the actual field value will be compared with.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oValues :: Lens.Lens' Operator (Lude.Maybe [Lude.Text])
oValues = Lens.lens (values :: Operator -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: Operator)
{-# DEPRECATED oValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The logical operation to be performed: equal (@EQ@ ), equal reference (@REF_EQ@ ), less than or equal (@LE@ ), greater than or equal (@GE@ ), or between (@BETWEEN@ ). Equal reference (@REF_EQ@ ) can be used only with reference fields. The other comparison types can be used only with String fields. The comparison types you can use apply only to certain object fields, as detailed below.
--
-- The comparison operators EQ and REF_EQ act on the following fields:
--
--     * name
--
--     * @sphere
--
--     * parent
--
--     * @componentParent
--
--     * @instanceParent
--
--     * @status
--
--     * @scheduledStartTime
--
--     * @scheduledEndTime
--
--     * @actualStartTime
--
--     * @actualEndTime
--
-- The comparison operators @GE@ , @LE@ , and @BETWEEN@ act on the following fields:
--
--     * @scheduledStartTime
--
--     * @scheduledEndTime
--
--     * @actualStartTime
--
--     * @actualEndTime
--
-- Note that fields beginning with the at sign (@) are read-only and set by the web service. When you name fields, you should choose names containing only alpha-numeric values, as symbols may be reserved by AWS Data Pipeline. User-defined fields that you add to a pipeline should prefix their name with the string "my".
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oType :: Lens.Lens' Operator (Lude.Maybe OperatorType)
oType = Lens.lens (type' :: Operator -> Lude.Maybe OperatorType) (\s a -> s {type' = a} :: Operator)
{-# DEPRECATED oType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.ToJSON Operator where
  toJSON Operator' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("values" Lude..=) Lude.<$> values,
            ("type" Lude..=) Lude.<$> type'
          ]
      )

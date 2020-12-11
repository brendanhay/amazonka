-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsResultAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsResultAttribute
  ( OpsResultAttribute (..),

    -- * Smart constructor
    mkOpsResultAttribute,

    -- * Lenses
    oraTypeName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The OpsItem data type to return.
--
-- /See:/ 'mkOpsResultAttribute' smart constructor.
newtype OpsResultAttribute = OpsResultAttribute'
  { typeName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OpsResultAttribute' with the minimum fields required to make a request.
--
-- * 'typeName' - Name of the data type. Valid value: AWS:OpsItem, AWS:EC2InstanceInformation, AWS:OpsItemTrendline, or AWS:ComplianceSummary.
mkOpsResultAttribute ::
  -- | 'typeName'
  Lude.Text ->
  OpsResultAttribute
mkOpsResultAttribute pTypeName_ =
  OpsResultAttribute' {typeName = pTypeName_}

-- | Name of the data type. Valid value: AWS:OpsItem, AWS:EC2InstanceInformation, AWS:OpsItemTrendline, or AWS:ComplianceSummary.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oraTypeName :: Lens.Lens' OpsResultAttribute Lude.Text
oraTypeName = Lens.lens (typeName :: OpsResultAttribute -> Lude.Text) (\s a -> s {typeName = a} :: OpsResultAttribute)
{-# DEPRECATED oraTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

instance Lude.ToJSON OpsResultAttribute where
  toJSON OpsResultAttribute' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("TypeName" Lude..= typeName)])

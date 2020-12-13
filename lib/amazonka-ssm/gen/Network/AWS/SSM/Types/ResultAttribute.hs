{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResultAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResultAttribute
  ( ResultAttribute (..),

    -- * Smart constructor
    mkResultAttribute,

    -- * Lenses
    raTypeName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The inventory item result attribute.
--
-- /See:/ 'mkResultAttribute' smart constructor.
newtype ResultAttribute = ResultAttribute'
  { -- | Name of the inventory item type. Valid value: AWS:InstanceInformation. Default Value: AWS:InstanceInformation.
    typeName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResultAttribute' with the minimum fields required to make a request.
--
-- * 'typeName' - Name of the inventory item type. Valid value: AWS:InstanceInformation. Default Value: AWS:InstanceInformation.
mkResultAttribute ::
  -- | 'typeName'
  Lude.Text ->
  ResultAttribute
mkResultAttribute pTypeName_ =
  ResultAttribute' {typeName = pTypeName_}

-- | Name of the inventory item type. Valid value: AWS:InstanceInformation. Default Value: AWS:InstanceInformation.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raTypeName :: Lens.Lens' ResultAttribute Lude.Text
raTypeName = Lens.lens (typeName :: ResultAttribute -> Lude.Text) (\s a -> s {typeName = a} :: ResultAttribute)
{-# DEPRECATED raTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

instance Lude.ToJSON ResultAttribute where
  toJSON ResultAttribute' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("TypeName" Lude..= typeName)])

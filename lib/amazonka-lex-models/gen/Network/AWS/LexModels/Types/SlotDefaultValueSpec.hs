{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.SlotDefaultValueSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.SlotDefaultValueSpec
  ( SlotDefaultValueSpec (..),

    -- * Smart constructor
    mkSlotDefaultValueSpec,

    -- * Lenses
    sdvsDefaultValueList,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.SlotDefaultValue
import qualified Network.AWS.Prelude as Lude

-- | Contains the default values for a slot. Default values are used when Amazon Lex hasn't determined a value for a slot.
--
-- /See:/ 'mkSlotDefaultValueSpec' smart constructor.
newtype SlotDefaultValueSpec = SlotDefaultValueSpec'
  { -- | The default values for a slot. You can specify more than one default. For example, you can specify a default value to use from a matching context variable, a session attribute, or a fixed value.
    --
    -- The default value chosen is selected based on the order that you specify them in the list. For example, if you specify a context variable and a fixed value in that order, Amazon Lex uses the context variable if it is available, else it uses the fixed value.
    defaultValueList :: [SlotDefaultValue]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SlotDefaultValueSpec' with the minimum fields required to make a request.
--
-- * 'defaultValueList' - The default values for a slot. You can specify more than one default. For example, you can specify a default value to use from a matching context variable, a session attribute, or a fixed value.
--
-- The default value chosen is selected based on the order that you specify them in the list. For example, if you specify a context variable and a fixed value in that order, Amazon Lex uses the context variable if it is available, else it uses the fixed value.
mkSlotDefaultValueSpec ::
  SlotDefaultValueSpec
mkSlotDefaultValueSpec =
  SlotDefaultValueSpec' {defaultValueList = Lude.mempty}

-- | The default values for a slot. You can specify more than one default. For example, you can specify a default value to use from a matching context variable, a session attribute, or a fixed value.
--
-- The default value chosen is selected based on the order that you specify them in the list. For example, if you specify a context variable and a fixed value in that order, Amazon Lex uses the context variable if it is available, else it uses the fixed value.
--
-- /Note:/ Consider using 'defaultValueList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdvsDefaultValueList :: Lens.Lens' SlotDefaultValueSpec [SlotDefaultValue]
sdvsDefaultValueList = Lens.lens (defaultValueList :: SlotDefaultValueSpec -> [SlotDefaultValue]) (\s a -> s {defaultValueList = a} :: SlotDefaultValueSpec)
{-# DEPRECATED sdvsDefaultValueList "Use generic-lens or generic-optics with 'defaultValueList' instead." #-}

instance Lude.FromJSON SlotDefaultValueSpec where
  parseJSON =
    Lude.withObject
      "SlotDefaultValueSpec"
      ( \x ->
          SlotDefaultValueSpec'
            Lude.<$> (x Lude..:? "defaultValueList" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON SlotDefaultValueSpec where
  toJSON SlotDefaultValueSpec' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("defaultValueList" Lude..= defaultValueList)]
      )

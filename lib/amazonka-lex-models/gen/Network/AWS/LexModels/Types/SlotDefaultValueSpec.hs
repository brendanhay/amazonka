{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.SlotDefaultValueSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.SlotDefaultValueSpec
  ( SlotDefaultValueSpec (..)
  -- * Smart constructor
  , mkSlotDefaultValueSpec
  -- * Lenses
  , sdvsDefaultValueList
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.SlotDefaultValue as Types
import qualified Network.AWS.Prelude as Core

-- | Contains the default values for a slot. Default values are used when Amazon Lex hasn't determined a value for a slot.
--
-- /See:/ 'mkSlotDefaultValueSpec' smart constructor.
newtype SlotDefaultValueSpec = SlotDefaultValueSpec'
  { defaultValueList :: [Types.SlotDefaultValue]
    -- ^ The default values for a slot. You can specify more than one default. For example, you can specify a default value to use from a matching context variable, a session attribute, or a fixed value.
--
-- The default value chosen is selected based on the order that you specify them in the list. For example, if you specify a context variable and a fixed value in that order, Amazon Lex uses the context variable if it is available, else it uses the fixed value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SlotDefaultValueSpec' value with any optional fields omitted.
mkSlotDefaultValueSpec
    :: SlotDefaultValueSpec
mkSlotDefaultValueSpec
  = SlotDefaultValueSpec'{defaultValueList = Core.mempty}

-- | The default values for a slot. You can specify more than one default. For example, you can specify a default value to use from a matching context variable, a session attribute, or a fixed value.
--
-- The default value chosen is selected based on the order that you specify them in the list. For example, if you specify a context variable and a fixed value in that order, Amazon Lex uses the context variable if it is available, else it uses the fixed value.
--
-- /Note:/ Consider using 'defaultValueList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdvsDefaultValueList :: Lens.Lens' SlotDefaultValueSpec [Types.SlotDefaultValue]
sdvsDefaultValueList = Lens.field @"defaultValueList"
{-# INLINEABLE sdvsDefaultValueList #-}
{-# DEPRECATED defaultValueList "Use generic-lens or generic-optics with 'defaultValueList' instead"  #-}

instance Core.FromJSON SlotDefaultValueSpec where
        toJSON SlotDefaultValueSpec{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("defaultValueList" Core..= defaultValueList)])

instance Core.FromJSON SlotDefaultValueSpec where
        parseJSON
          = Core.withObject "SlotDefaultValueSpec" Core.$
              \ x ->
                SlotDefaultValueSpec' Core.<$>
                  (x Core..:? "defaultValueList" Core..!= Core.mempty)

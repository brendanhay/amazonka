{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.SlotDefaultValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.SlotDefaultValue
  ( SlotDefaultValue (..)
  -- * Smart constructor
  , mkSlotDefaultValue
  -- * Lenses
  , sdvDefaultValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.DefaultValue as Types
import qualified Network.AWS.Prelude as Core

-- | A default value for a slot.
--
-- /See:/ 'mkSlotDefaultValue' smart constructor.
newtype SlotDefaultValue = SlotDefaultValue'
  { defaultValue :: Types.DefaultValue
    -- ^ The default value for the slot. You can specify one of the following:
--
--
--     * @#context-name.slot-name@ - The slot value "slot-name" in the context "context-name."
--
--
--     * @{attribute}@ - The slot value of the session attribute "attribute."
--
--
--     * @'value'@ - The discrete value "value."
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SlotDefaultValue' value with any optional fields omitted.
mkSlotDefaultValue
    :: Types.DefaultValue -- ^ 'defaultValue'
    -> SlotDefaultValue
mkSlotDefaultValue defaultValue = SlotDefaultValue'{defaultValue}

-- | The default value for the slot. You can specify one of the following:
--
--
--     * @#context-name.slot-name@ - The slot value "slot-name" in the context "context-name."
--
--
--     * @{attribute}@ - The slot value of the session attribute "attribute."
--
--
--     * @'value'@ - The discrete value "value."
--
--
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdvDefaultValue :: Lens.Lens' SlotDefaultValue Types.DefaultValue
sdvDefaultValue = Lens.field @"defaultValue"
{-# INLINEABLE sdvDefaultValue #-}
{-# DEPRECATED defaultValue "Use generic-lens or generic-optics with 'defaultValue' instead"  #-}

instance Core.FromJSON SlotDefaultValue where
        toJSON SlotDefaultValue{..}
          = Core.object
              (Core.catMaybes [Core.Just ("defaultValue" Core..= defaultValue)])

instance Core.FromJSON SlotDefaultValue where
        parseJSON
          = Core.withObject "SlotDefaultValue" Core.$
              \ x -> SlotDefaultValue' Core.<$> (x Core..: "defaultValue")

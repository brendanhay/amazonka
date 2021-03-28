{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Slot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.Slot
  ( Slot (..)
  -- * Smart constructor
  , mkSlot
  -- * Lenses
  , sfName
  , sfSlotConstraint
  , sfDefaultValueSpec
  , sfDescription
  , sfObfuscationSetting
  , sfPriority
  , sfResponseCard
  , sfSampleUtterances
  , sfSlotType
  , sfSlotTypeVersion
  , sfValueElicitationPrompt
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.CustomOrBuiltinSlotTypeName as Types
import qualified Network.AWS.LexModels.Types.Description as Types
import qualified Network.AWS.LexModels.Types.ObfuscationSetting as Types
import qualified Network.AWS.LexModels.Types.Prompt as Types
import qualified Network.AWS.LexModels.Types.ResponseCard as Types
import qualified Network.AWS.LexModels.Types.SlotConstraint as Types
import qualified Network.AWS.LexModels.Types.SlotDefaultValueSpec as Types
import qualified Network.AWS.LexModels.Types.SlotName as Types
import qualified Network.AWS.LexModels.Types.SlotTypeVersion as Types
import qualified Network.AWS.LexModels.Types.Utterance as Types
import qualified Network.AWS.Prelude as Core

-- | Identifies the version of a specific slot.
--
-- /See:/ 'mkSlot' smart constructor.
data Slot = Slot'
  { name :: Types.SlotName
    -- ^ The name of the slot.
  , slotConstraint :: Types.SlotConstraint
    -- ^ Specifies whether the slot is required or optional. 
  , defaultValueSpec :: Core.Maybe Types.SlotDefaultValueSpec
    -- ^ A list of default values for the slot. Default values are used when Amazon Lex hasn't determined a value for a slot. You can specify default values from context variables, session attributes, and defined values.
  , description :: Core.Maybe Types.Description
    -- ^ A description of the slot.
  , obfuscationSetting :: Core.Maybe Types.ObfuscationSetting
    -- ^ Determines whether a slot is obfuscated in conversation logs and stored utterances. When you obfuscate a slot, the value is replaced by the slot name in curly braces ({}). For example, if the slot name is "full_name", obfuscated values are replaced with "{full_name}". For more information, see <https://docs.aws.amazon.com/lex/latest/dg/how-obfuscate.html Slot Obfuscation > . 
  , priority :: Core.Maybe Core.Natural
    -- ^ Directs Amazon Lex the order in which to elicit this slot value from the user. For example, if the intent has two slots with priorities 1 and 2, AWS Amazon Lex first elicits a value for the slot with priority 1.
--
-- If multiple slots share the same priority, the order in which Amazon Lex elicits values is arbitrary.
  , responseCard :: Core.Maybe Types.ResponseCard
    -- ^ A set of possible responses for the slot type used by text-based clients. A user chooses an option from the response card, instead of using text to reply. 
  , sampleUtterances :: Core.Maybe [Types.Utterance]
    -- ^ If you know a specific pattern with which users might respond to an Amazon Lex request for a slot value, you can provide those utterances to improve accuracy. This is optional. In most cases, Amazon Lex is capable of understanding user utterances. 
  , slotType :: Core.Maybe Types.CustomOrBuiltinSlotTypeName
    -- ^ The type of the slot, either a custom slot type that you defined or one of the built-in slot types.
  , slotTypeVersion :: Core.Maybe Types.SlotTypeVersion
    -- ^ The version of the slot type.
  , valueElicitationPrompt :: Core.Maybe Types.Prompt
    -- ^ The prompt that Amazon Lex uses to elicit the slot value from the user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Slot' value with any optional fields omitted.
mkSlot
    :: Types.SlotName -- ^ 'name'
    -> Types.SlotConstraint -- ^ 'slotConstraint'
    -> Slot
mkSlot name slotConstraint
  = Slot'{name, slotConstraint, defaultValueSpec = Core.Nothing,
          description = Core.Nothing, obfuscationSetting = Core.Nothing,
          priority = Core.Nothing, responseCard = Core.Nothing,
          sampleUtterances = Core.Nothing, slotType = Core.Nothing,
          slotTypeVersion = Core.Nothing,
          valueElicitationPrompt = Core.Nothing}

-- | The name of the slot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfName :: Lens.Lens' Slot Types.SlotName
sfName = Lens.field @"name"
{-# INLINEABLE sfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Specifies whether the slot is required or optional. 
--
-- /Note:/ Consider using 'slotConstraint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfSlotConstraint :: Lens.Lens' Slot Types.SlotConstraint
sfSlotConstraint = Lens.field @"slotConstraint"
{-# INLINEABLE sfSlotConstraint #-}
{-# DEPRECATED slotConstraint "Use generic-lens or generic-optics with 'slotConstraint' instead"  #-}

-- | A list of default values for the slot. Default values are used when Amazon Lex hasn't determined a value for a slot. You can specify default values from context variables, session attributes, and defined values.
--
-- /Note:/ Consider using 'defaultValueSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDefaultValueSpec :: Lens.Lens' Slot (Core.Maybe Types.SlotDefaultValueSpec)
sfDefaultValueSpec = Lens.field @"defaultValueSpec"
{-# INLINEABLE sfDefaultValueSpec #-}
{-# DEPRECATED defaultValueSpec "Use generic-lens or generic-optics with 'defaultValueSpec' instead"  #-}

-- | A description of the slot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDescription :: Lens.Lens' Slot (Core.Maybe Types.Description)
sfDescription = Lens.field @"description"
{-# INLINEABLE sfDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Determines whether a slot is obfuscated in conversation logs and stored utterances. When you obfuscate a slot, the value is replaced by the slot name in curly braces ({}). For example, if the slot name is "full_name", obfuscated values are replaced with "{full_name}". For more information, see <https://docs.aws.amazon.com/lex/latest/dg/how-obfuscate.html Slot Obfuscation > . 
--
-- /Note:/ Consider using 'obfuscationSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfObfuscationSetting :: Lens.Lens' Slot (Core.Maybe Types.ObfuscationSetting)
sfObfuscationSetting = Lens.field @"obfuscationSetting"
{-# INLINEABLE sfObfuscationSetting #-}
{-# DEPRECATED obfuscationSetting "Use generic-lens or generic-optics with 'obfuscationSetting' instead"  #-}

-- | Directs Amazon Lex the order in which to elicit this slot value from the user. For example, if the intent has two slots with priorities 1 and 2, AWS Amazon Lex first elicits a value for the slot with priority 1.
--
-- If multiple slots share the same priority, the order in which Amazon Lex elicits values is arbitrary.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfPriority :: Lens.Lens' Slot (Core.Maybe Core.Natural)
sfPriority = Lens.field @"priority"
{-# INLINEABLE sfPriority #-}
{-# DEPRECATED priority "Use generic-lens or generic-optics with 'priority' instead"  #-}

-- | A set of possible responses for the slot type used by text-based clients. A user chooses an option from the response card, instead of using text to reply. 
--
-- /Note:/ Consider using 'responseCard' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfResponseCard :: Lens.Lens' Slot (Core.Maybe Types.ResponseCard)
sfResponseCard = Lens.field @"responseCard"
{-# INLINEABLE sfResponseCard #-}
{-# DEPRECATED responseCard "Use generic-lens or generic-optics with 'responseCard' instead"  #-}

-- | If you know a specific pattern with which users might respond to an Amazon Lex request for a slot value, you can provide those utterances to improve accuracy. This is optional. In most cases, Amazon Lex is capable of understanding user utterances. 
--
-- /Note:/ Consider using 'sampleUtterances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfSampleUtterances :: Lens.Lens' Slot (Core.Maybe [Types.Utterance])
sfSampleUtterances = Lens.field @"sampleUtterances"
{-# INLINEABLE sfSampleUtterances #-}
{-# DEPRECATED sampleUtterances "Use generic-lens or generic-optics with 'sampleUtterances' instead"  #-}

-- | The type of the slot, either a custom slot type that you defined or one of the built-in slot types.
--
-- /Note:/ Consider using 'slotType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfSlotType :: Lens.Lens' Slot (Core.Maybe Types.CustomOrBuiltinSlotTypeName)
sfSlotType = Lens.field @"slotType"
{-# INLINEABLE sfSlotType #-}
{-# DEPRECATED slotType "Use generic-lens or generic-optics with 'slotType' instead"  #-}

-- | The version of the slot type.
--
-- /Note:/ Consider using 'slotTypeVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfSlotTypeVersion :: Lens.Lens' Slot (Core.Maybe Types.SlotTypeVersion)
sfSlotTypeVersion = Lens.field @"slotTypeVersion"
{-# INLINEABLE sfSlotTypeVersion #-}
{-# DEPRECATED slotTypeVersion "Use generic-lens or generic-optics with 'slotTypeVersion' instead"  #-}

-- | The prompt that Amazon Lex uses to elicit the slot value from the user.
--
-- /Note:/ Consider using 'valueElicitationPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfValueElicitationPrompt :: Lens.Lens' Slot (Core.Maybe Types.Prompt)
sfValueElicitationPrompt = Lens.field @"valueElicitationPrompt"
{-# INLINEABLE sfValueElicitationPrompt #-}
{-# DEPRECATED valueElicitationPrompt "Use generic-lens or generic-optics with 'valueElicitationPrompt' instead"  #-}

instance Core.FromJSON Slot where
        toJSON Slot{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just ("slotConstraint" Core..= slotConstraint),
                  ("defaultValueSpec" Core..=) Core.<$> defaultValueSpec,
                  ("description" Core..=) Core.<$> description,
                  ("obfuscationSetting" Core..=) Core.<$> obfuscationSetting,
                  ("priority" Core..=) Core.<$> priority,
                  ("responseCard" Core..=) Core.<$> responseCard,
                  ("sampleUtterances" Core..=) Core.<$> sampleUtterances,
                  ("slotType" Core..=) Core.<$> slotType,
                  ("slotTypeVersion" Core..=) Core.<$> slotTypeVersion,
                  ("valueElicitationPrompt" Core..=) Core.<$>
                    valueElicitationPrompt])

instance Core.FromJSON Slot where
        parseJSON
          = Core.withObject "Slot" Core.$
              \ x ->
                Slot' Core.<$>
                  (x Core..: "name") Core.<*> x Core..: "slotConstraint" Core.<*>
                    x Core..:? "defaultValueSpec"
                    Core.<*> x Core..:? "description"
                    Core.<*> x Core..:? "obfuscationSetting"
                    Core.<*> x Core..:? "priority"
                    Core.<*> x Core..:? "responseCard"
                    Core.<*> x Core..:? "sampleUtterances"
                    Core.<*> x Core..:? "slotType"
                    Core.<*> x Core..:? "slotTypeVersion"
                    Core.<*> x Core..:? "valueElicitationPrompt"

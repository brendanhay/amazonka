-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Slot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.Slot
  ( Slot (..),

    -- * Smart constructor
    mkSlot,

    -- * Lenses
    sSlotType,
    sValueElicitationPrompt,
    sResponseCard,
    sPriority,
    sObfuscationSetting,
    sDefaultValueSpec,
    sSlotTypeVersion,
    sSampleUtterances,
    sDescription,
    sName,
    sSlotConstraint,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.ObfuscationSetting
import Network.AWS.LexModels.Types.Prompt
import Network.AWS.LexModels.Types.SlotConstraint
import Network.AWS.LexModels.Types.SlotDefaultValueSpec
import qualified Network.AWS.Prelude as Lude

-- | Identifies the version of a specific slot.
--
-- /See:/ 'mkSlot' smart constructor.
data Slot = Slot'
  { slotType :: Lude.Maybe Lude.Text,
    valueElicitationPrompt :: Lude.Maybe Prompt,
    responseCard :: Lude.Maybe Lude.Text,
    priority :: Lude.Maybe Lude.Natural,
    obfuscationSetting :: Lude.Maybe ObfuscationSetting,
    defaultValueSpec :: Lude.Maybe SlotDefaultValueSpec,
    slotTypeVersion :: Lude.Maybe Lude.Text,
    sampleUtterances :: Lude.Maybe [Lude.Text],
    description :: Lude.Maybe Lude.Text,
    name :: Lude.Text,
    slotConstraint :: SlotConstraint
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Slot' with the minimum fields required to make a request.
--
-- * 'defaultValueSpec' - A list of default values for the slot. Default values are used when Amazon Lex hasn't determined a value for a slot. You can specify default values from context variables, session attributes, and defined values.
-- * 'description' - A description of the slot.
-- * 'name' - The name of the slot.
-- * 'obfuscationSetting' - Determines whether a slot is obfuscated in conversation logs and stored utterances. When you obfuscate a slot, the value is replaced by the slot name in curly braces ({}). For example, if the slot name is "full_name", obfuscated values are replaced with "{full_name}". For more information, see <https://docs.aws.amazon.com/lex/latest/dg/how-obfuscate.html Slot Obfuscation > .
-- * 'priority' - Directs Amazon Lex the order in which to elicit this slot value from the user. For example, if the intent has two slots with priorities 1 and 2, AWS Amazon Lex first elicits a value for the slot with priority 1.
--
-- If multiple slots share the same priority, the order in which Amazon Lex elicits values is arbitrary.
-- * 'responseCard' - A set of possible responses for the slot type used by text-based clients. A user chooses an option from the response card, instead of using text to reply.
-- * 'sampleUtterances' - If you know a specific pattern with which users might respond to an Amazon Lex request for a slot value, you can provide those utterances to improve accuracy. This is optional. In most cases, Amazon Lex is capable of understanding user utterances.
-- * 'slotConstraint' - Specifies whether the slot is required or optional.
-- * 'slotType' - The type of the slot, either a custom slot type that you defined or one of the built-in slot types.
-- * 'slotTypeVersion' - The version of the slot type.
-- * 'valueElicitationPrompt' - The prompt that Amazon Lex uses to elicit the slot value from the user.
mkSlot ::
  -- | 'name'
  Lude.Text ->
  -- | 'slotConstraint'
  SlotConstraint ->
  Slot
mkSlot pName_ pSlotConstraint_ =
  Slot'
    { slotType = Lude.Nothing,
      valueElicitationPrompt = Lude.Nothing,
      responseCard = Lude.Nothing,
      priority = Lude.Nothing,
      obfuscationSetting = Lude.Nothing,
      defaultValueSpec = Lude.Nothing,
      slotTypeVersion = Lude.Nothing,
      sampleUtterances = Lude.Nothing,
      description = Lude.Nothing,
      name = pName_,
      slotConstraint = pSlotConstraint_
    }

-- | The type of the slot, either a custom slot type that you defined or one of the built-in slot types.
--
-- /Note:/ Consider using 'slotType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSlotType :: Lens.Lens' Slot (Lude.Maybe Lude.Text)
sSlotType = Lens.lens (slotType :: Slot -> Lude.Maybe Lude.Text) (\s a -> s {slotType = a} :: Slot)
{-# DEPRECATED sSlotType "Use generic-lens or generic-optics with 'slotType' instead." #-}

-- | The prompt that Amazon Lex uses to elicit the slot value from the user.
--
-- /Note:/ Consider using 'valueElicitationPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sValueElicitationPrompt :: Lens.Lens' Slot (Lude.Maybe Prompt)
sValueElicitationPrompt = Lens.lens (valueElicitationPrompt :: Slot -> Lude.Maybe Prompt) (\s a -> s {valueElicitationPrompt = a} :: Slot)
{-# DEPRECATED sValueElicitationPrompt "Use generic-lens or generic-optics with 'valueElicitationPrompt' instead." #-}

-- | A set of possible responses for the slot type used by text-based clients. A user chooses an option from the response card, instead of using text to reply.
--
-- /Note:/ Consider using 'responseCard' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sResponseCard :: Lens.Lens' Slot (Lude.Maybe Lude.Text)
sResponseCard = Lens.lens (responseCard :: Slot -> Lude.Maybe Lude.Text) (\s a -> s {responseCard = a} :: Slot)
{-# DEPRECATED sResponseCard "Use generic-lens or generic-optics with 'responseCard' instead." #-}

-- | Directs Amazon Lex the order in which to elicit this slot value from the user. For example, if the intent has two slots with priorities 1 and 2, AWS Amazon Lex first elicits a value for the slot with priority 1.
--
-- If multiple slots share the same priority, the order in which Amazon Lex elicits values is arbitrary.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPriority :: Lens.Lens' Slot (Lude.Maybe Lude.Natural)
sPriority = Lens.lens (priority :: Slot -> Lude.Maybe Lude.Natural) (\s a -> s {priority = a} :: Slot)
{-# DEPRECATED sPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | Determines whether a slot is obfuscated in conversation logs and stored utterances. When you obfuscate a slot, the value is replaced by the slot name in curly braces ({}). For example, if the slot name is "full_name", obfuscated values are replaced with "{full_name}". For more information, see <https://docs.aws.amazon.com/lex/latest/dg/how-obfuscate.html Slot Obfuscation > .
--
-- /Note:/ Consider using 'obfuscationSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sObfuscationSetting :: Lens.Lens' Slot (Lude.Maybe ObfuscationSetting)
sObfuscationSetting = Lens.lens (obfuscationSetting :: Slot -> Lude.Maybe ObfuscationSetting) (\s a -> s {obfuscationSetting = a} :: Slot)
{-# DEPRECATED sObfuscationSetting "Use generic-lens or generic-optics with 'obfuscationSetting' instead." #-}

-- | A list of default values for the slot. Default values are used when Amazon Lex hasn't determined a value for a slot. You can specify default values from context variables, session attributes, and defined values.
--
-- /Note:/ Consider using 'defaultValueSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDefaultValueSpec :: Lens.Lens' Slot (Lude.Maybe SlotDefaultValueSpec)
sDefaultValueSpec = Lens.lens (defaultValueSpec :: Slot -> Lude.Maybe SlotDefaultValueSpec) (\s a -> s {defaultValueSpec = a} :: Slot)
{-# DEPRECATED sDefaultValueSpec "Use generic-lens or generic-optics with 'defaultValueSpec' instead." #-}

-- | The version of the slot type.
--
-- /Note:/ Consider using 'slotTypeVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSlotTypeVersion :: Lens.Lens' Slot (Lude.Maybe Lude.Text)
sSlotTypeVersion = Lens.lens (slotTypeVersion :: Slot -> Lude.Maybe Lude.Text) (\s a -> s {slotTypeVersion = a} :: Slot)
{-# DEPRECATED sSlotTypeVersion "Use generic-lens or generic-optics with 'slotTypeVersion' instead." #-}

-- | If you know a specific pattern with which users might respond to an Amazon Lex request for a slot value, you can provide those utterances to improve accuracy. This is optional. In most cases, Amazon Lex is capable of understanding user utterances.
--
-- /Note:/ Consider using 'sampleUtterances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSampleUtterances :: Lens.Lens' Slot (Lude.Maybe [Lude.Text])
sSampleUtterances = Lens.lens (sampleUtterances :: Slot -> Lude.Maybe [Lude.Text]) (\s a -> s {sampleUtterances = a} :: Slot)
{-# DEPRECATED sSampleUtterances "Use generic-lens or generic-optics with 'sampleUtterances' instead." #-}

-- | A description of the slot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDescription :: Lens.Lens' Slot (Lude.Maybe Lude.Text)
sDescription = Lens.lens (description :: Slot -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Slot)
{-# DEPRECATED sDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the slot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' Slot Lude.Text
sName = Lens.lens (name :: Slot -> Lude.Text) (\s a -> s {name = a} :: Slot)
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies whether the slot is required or optional.
--
-- /Note:/ Consider using 'slotConstraint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSlotConstraint :: Lens.Lens' Slot SlotConstraint
sSlotConstraint = Lens.lens (slotConstraint :: Slot -> SlotConstraint) (\s a -> s {slotConstraint = a} :: Slot)
{-# DEPRECATED sSlotConstraint "Use generic-lens or generic-optics with 'slotConstraint' instead." #-}

instance Lude.FromJSON Slot where
  parseJSON =
    Lude.withObject
      "Slot"
      ( \x ->
          Slot'
            Lude.<$> (x Lude..:? "slotType")
            Lude.<*> (x Lude..:? "valueElicitationPrompt")
            Lude.<*> (x Lude..:? "responseCard")
            Lude.<*> (x Lude..:? "priority")
            Lude.<*> (x Lude..:? "obfuscationSetting")
            Lude.<*> (x Lude..:? "defaultValueSpec")
            Lude.<*> (x Lude..:? "slotTypeVersion")
            Lude.<*> (x Lude..:? "sampleUtterances" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..: "slotConstraint")
      )

instance Lude.ToJSON Slot where
  toJSON Slot' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("slotType" Lude..=) Lude.<$> slotType,
            ("valueElicitationPrompt" Lude..=) Lude.<$> valueElicitationPrompt,
            ("responseCard" Lude..=) Lude.<$> responseCard,
            ("priority" Lude..=) Lude.<$> priority,
            ("obfuscationSetting" Lude..=) Lude.<$> obfuscationSetting,
            ("defaultValueSpec" Lude..=) Lude.<$> defaultValueSpec,
            ("slotTypeVersion" Lude..=) Lude.<$> slotTypeVersion,
            ("sampleUtterances" Lude..=) Lude.<$> sampleUtterances,
            ("description" Lude..=) Lude.<$> description,
            Lude.Just ("name" Lude..= name),
            Lude.Just ("slotConstraint" Lude..= slotConstraint)
          ]
      )

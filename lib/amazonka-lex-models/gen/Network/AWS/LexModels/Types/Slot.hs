{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Slot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.Slot where

import Network.AWS.Lens
import Network.AWS.LexModels.Types.ObfuscationSetting
import Network.AWS.LexModels.Types.Prompt
import Network.AWS.LexModels.Types.SlotConstraint
import Network.AWS.LexModels.Types.SlotDefaultValueSpec
import Network.AWS.Prelude

-- | Identifies the version of a specific slot.
--
--
--
-- /See:/ 'slot' smart constructor.
data Slot = Slot'
  { _sSlotType :: !(Maybe Text),
    _sValueElicitationPrompt :: !(Maybe Prompt),
    _sResponseCard :: !(Maybe Text),
    _sPriority :: !(Maybe Nat),
    _sObfuscationSetting :: !(Maybe ObfuscationSetting),
    _sDefaultValueSpec :: !(Maybe SlotDefaultValueSpec),
    _sSlotTypeVersion :: !(Maybe Text),
    _sSampleUtterances :: !(Maybe [Text]),
    _sDescription :: !(Maybe Text),
    _sName :: !Text,
    _sSlotConstraint :: !SlotConstraint
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Slot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSlotType' - The type of the slot, either a custom slot type that you defined or one of the built-in slot types.
--
-- * 'sValueElicitationPrompt' - The prompt that Amazon Lex uses to elicit the slot value from the user.
--
-- * 'sResponseCard' - A set of possible responses for the slot type used by text-based clients. A user chooses an option from the response card, instead of using text to reply.
--
-- * 'sPriority' - Directs Amazon Lex the order in which to elicit this slot value from the user. For example, if the intent has two slots with priorities 1 and 2, AWS Amazon Lex first elicits a value for the slot with priority 1. If multiple slots share the same priority, the order in which Amazon Lex elicits values is arbitrary.
--
-- * 'sObfuscationSetting' - Determines whether a slot is obfuscated in conversation logs and stored utterances. When you obfuscate a slot, the value is replaced by the slot name in curly braces ({}). For example, if the slot name is "full_name", obfuscated values are replaced with "{full_name}". For more information, see <https://docs.aws.amazon.com/lex/latest/dg/how-obfuscate.html Slot Obfuscation > .
--
-- * 'sDefaultValueSpec' - A list of default values for the slot. Default values are used when Amazon Lex hasn't determined a value for a slot. You can specify default values from context variables, session attributes, and defined values.
--
-- * 'sSlotTypeVersion' - The version of the slot type.
--
-- * 'sSampleUtterances' - If you know a specific pattern with which users might respond to an Amazon Lex request for a slot value, you can provide those utterances to improve accuracy. This is optional. In most cases, Amazon Lex is capable of understanding user utterances.
--
-- * 'sDescription' - A description of the slot.
--
-- * 'sName' - The name of the slot.
--
-- * 'sSlotConstraint' - Specifies whether the slot is required or optional.
slot ::
  -- | 'sName'
  Text ->
  -- | 'sSlotConstraint'
  SlotConstraint ->
  Slot
slot pName_ pSlotConstraint_ =
  Slot'
    { _sSlotType = Nothing,
      _sValueElicitationPrompt = Nothing,
      _sResponseCard = Nothing,
      _sPriority = Nothing,
      _sObfuscationSetting = Nothing,
      _sDefaultValueSpec = Nothing,
      _sSlotTypeVersion = Nothing,
      _sSampleUtterances = Nothing,
      _sDescription = Nothing,
      _sName = pName_,
      _sSlotConstraint = pSlotConstraint_
    }

-- | The type of the slot, either a custom slot type that you defined or one of the built-in slot types.
sSlotType :: Lens' Slot (Maybe Text)
sSlotType = lens _sSlotType (\s a -> s {_sSlotType = a})

-- | The prompt that Amazon Lex uses to elicit the slot value from the user.
sValueElicitationPrompt :: Lens' Slot (Maybe Prompt)
sValueElicitationPrompt = lens _sValueElicitationPrompt (\s a -> s {_sValueElicitationPrompt = a})

-- | A set of possible responses for the slot type used by text-based clients. A user chooses an option from the response card, instead of using text to reply.
sResponseCard :: Lens' Slot (Maybe Text)
sResponseCard = lens _sResponseCard (\s a -> s {_sResponseCard = a})

-- | Directs Amazon Lex the order in which to elicit this slot value from the user. For example, if the intent has two slots with priorities 1 and 2, AWS Amazon Lex first elicits a value for the slot with priority 1. If multiple slots share the same priority, the order in which Amazon Lex elicits values is arbitrary.
sPriority :: Lens' Slot (Maybe Natural)
sPriority = lens _sPriority (\s a -> s {_sPriority = a}) . mapping _Nat

-- | Determines whether a slot is obfuscated in conversation logs and stored utterances. When you obfuscate a slot, the value is replaced by the slot name in curly braces ({}). For example, if the slot name is "full_name", obfuscated values are replaced with "{full_name}". For more information, see <https://docs.aws.amazon.com/lex/latest/dg/how-obfuscate.html Slot Obfuscation > .
sObfuscationSetting :: Lens' Slot (Maybe ObfuscationSetting)
sObfuscationSetting = lens _sObfuscationSetting (\s a -> s {_sObfuscationSetting = a})

-- | A list of default values for the slot. Default values are used when Amazon Lex hasn't determined a value for a slot. You can specify default values from context variables, session attributes, and defined values.
sDefaultValueSpec :: Lens' Slot (Maybe SlotDefaultValueSpec)
sDefaultValueSpec = lens _sDefaultValueSpec (\s a -> s {_sDefaultValueSpec = a})

-- | The version of the slot type.
sSlotTypeVersion :: Lens' Slot (Maybe Text)
sSlotTypeVersion = lens _sSlotTypeVersion (\s a -> s {_sSlotTypeVersion = a})

-- | If you know a specific pattern with which users might respond to an Amazon Lex request for a slot value, you can provide those utterances to improve accuracy. This is optional. In most cases, Amazon Lex is capable of understanding user utterances.
sSampleUtterances :: Lens' Slot [Text]
sSampleUtterances = lens _sSampleUtterances (\s a -> s {_sSampleUtterances = a}) . _Default . _Coerce

-- | A description of the slot.
sDescription :: Lens' Slot (Maybe Text)
sDescription = lens _sDescription (\s a -> s {_sDescription = a})

-- | The name of the slot.
sName :: Lens' Slot Text
sName = lens _sName (\s a -> s {_sName = a})

-- | Specifies whether the slot is required or optional.
sSlotConstraint :: Lens' Slot SlotConstraint
sSlotConstraint = lens _sSlotConstraint (\s a -> s {_sSlotConstraint = a})

instance FromJSON Slot where
  parseJSON =
    withObject
      "Slot"
      ( \x ->
          Slot'
            <$> (x .:? "slotType")
            <*> (x .:? "valueElicitationPrompt")
            <*> (x .:? "responseCard")
            <*> (x .:? "priority")
            <*> (x .:? "obfuscationSetting")
            <*> (x .:? "defaultValueSpec")
            <*> (x .:? "slotTypeVersion")
            <*> (x .:? "sampleUtterances" .!= mempty)
            <*> (x .:? "description")
            <*> (x .: "name")
            <*> (x .: "slotConstraint")
      )

instance Hashable Slot

instance NFData Slot

instance ToJSON Slot where
  toJSON Slot' {..} =
    object
      ( catMaybes
          [ ("slotType" .=) <$> _sSlotType,
            ("valueElicitationPrompt" .=) <$> _sValueElicitationPrompt,
            ("responseCard" .=) <$> _sResponseCard,
            ("priority" .=) <$> _sPriority,
            ("obfuscationSetting" .=) <$> _sObfuscationSetting,
            ("defaultValueSpec" .=) <$> _sDefaultValueSpec,
            ("slotTypeVersion" .=) <$> _sSlotTypeVersion,
            ("sampleUtterances" .=) <$> _sSampleUtterances,
            ("description" .=) <$> _sDescription,
            Just ("name" .= _sName),
            Just ("slotConstraint" .= _sSlotConstraint)
          ]
      )

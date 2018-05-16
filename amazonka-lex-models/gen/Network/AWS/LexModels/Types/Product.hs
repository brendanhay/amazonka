{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.Product where

import Network.AWS.Lens
import Network.AWS.LexModels.Types.Sum
import Network.AWS.Prelude

-- | Provides information about a bot alias.
--
--
--
-- /See:/ 'botAliasMetadata' smart constructor.
data BotAliasMetadata = BotAliasMetadata'
  { _bamChecksum        :: !(Maybe Text)
  , _bamBotVersion      :: !(Maybe Text)
  , _bamBotName         :: !(Maybe Text)
  , _bamCreatedDate     :: !(Maybe POSIX)
  , _bamName            :: !(Maybe Text)
  , _bamLastUpdatedDate :: !(Maybe POSIX)
  , _bamDescription     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BotAliasMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bamChecksum' - Checksum of the bot alias.
--
-- * 'bamBotVersion' - The version of the Amazon Lex bot to which the alias points.
--
-- * 'bamBotName' - The name of the bot to which the alias points.
--
-- * 'bamCreatedDate' - The date that the bot alias was created.
--
-- * 'bamName' - The name of the bot alias.
--
-- * 'bamLastUpdatedDate' - The date that the bot alias was updated. When you create a resource, the creation date and last updated date are the same.
--
-- * 'bamDescription' - A description of the bot alias.
botAliasMetadata
    :: BotAliasMetadata
botAliasMetadata =
  BotAliasMetadata'
    { _bamChecksum = Nothing
    , _bamBotVersion = Nothing
    , _bamBotName = Nothing
    , _bamCreatedDate = Nothing
    , _bamName = Nothing
    , _bamLastUpdatedDate = Nothing
    , _bamDescription = Nothing
    }


-- | Checksum of the bot alias.
bamChecksum :: Lens' BotAliasMetadata (Maybe Text)
bamChecksum = lens _bamChecksum (\ s a -> s{_bamChecksum = a})

-- | The version of the Amazon Lex bot to which the alias points.
bamBotVersion :: Lens' BotAliasMetadata (Maybe Text)
bamBotVersion = lens _bamBotVersion (\ s a -> s{_bamBotVersion = a})

-- | The name of the bot to which the alias points.
bamBotName :: Lens' BotAliasMetadata (Maybe Text)
bamBotName = lens _bamBotName (\ s a -> s{_bamBotName = a})

-- | The date that the bot alias was created.
bamCreatedDate :: Lens' BotAliasMetadata (Maybe UTCTime)
bamCreatedDate = lens _bamCreatedDate (\ s a -> s{_bamCreatedDate = a}) . mapping _Time

-- | The name of the bot alias.
bamName :: Lens' BotAliasMetadata (Maybe Text)
bamName = lens _bamName (\ s a -> s{_bamName = a})

-- | The date that the bot alias was updated. When you create a resource, the creation date and last updated date are the same.
bamLastUpdatedDate :: Lens' BotAliasMetadata (Maybe UTCTime)
bamLastUpdatedDate = lens _bamLastUpdatedDate (\ s a -> s{_bamLastUpdatedDate = a}) . mapping _Time

-- | A description of the bot alias.
bamDescription :: Lens' BotAliasMetadata (Maybe Text)
bamDescription = lens _bamDescription (\ s a -> s{_bamDescription = a})

instance FromJSON BotAliasMetadata where
        parseJSON
          = withObject "BotAliasMetadata"
              (\ x ->
                 BotAliasMetadata' <$>
                   (x .:? "checksum") <*> (x .:? "botVersion") <*>
                     (x .:? "botName")
                     <*> (x .:? "createdDate")
                     <*> (x .:? "name")
                     <*> (x .:? "lastUpdatedDate")
                     <*> (x .:? "description"))

instance Hashable BotAliasMetadata where

instance NFData BotAliasMetadata where

-- | Represents an association between an Amazon Lex bot and an external messaging platform.
--
--
--
-- /See:/ 'botChannelAssociation' smart constructor.
data BotChannelAssociation = BotChannelAssociation'
  { _bcaFailureReason    :: !(Maybe Text)
  , _bcaStatus           :: !(Maybe ChannelStatus)
  , _bcaBotAlias         :: !(Maybe Text)
  , _bcaBotName          :: !(Maybe Text)
  , _bcaBotConfiguration :: !(Maybe (Sensitive (Map Text Text)))
  , _bcaCreatedDate      :: !(Maybe POSIX)
  , _bcaName             :: !(Maybe Text)
  , _bcaType             :: !(Maybe ChannelType)
  , _bcaDescription      :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'BotChannelAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bcaFailureReason' - If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to create the association.
--
-- * 'bcaStatus' - The status of the bot channel.      * @CREATED@ - The channel has been created and is ready for use.     * @IN_PROGRESS@ - Channel creation is in progress.     * @FAILED@ - There was an error creating the channel. For information about the reason for the failure, see the @failureReason@ field.
--
-- * 'bcaBotAlias' - An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
--
-- * 'bcaBotName' - The name of the Amazon Lex bot to which this association is being made.
--
-- * 'bcaBotConfiguration' - Provides information necessary to communicate with the messaging platform.
--
-- * 'bcaCreatedDate' - The date that the association between the Amazon Lex bot and the channel was created.
--
-- * 'bcaName' - The name of the association between the bot and the channel.
--
-- * 'bcaType' - Specifies the type of association by indicating the type of channel being established between the Amazon Lex bot and the external messaging platform.
--
-- * 'bcaDescription' - A text description of the association you are creating.
botChannelAssociation
    :: BotChannelAssociation
botChannelAssociation =
  BotChannelAssociation'
    { _bcaFailureReason = Nothing
    , _bcaStatus = Nothing
    , _bcaBotAlias = Nothing
    , _bcaBotName = Nothing
    , _bcaBotConfiguration = Nothing
    , _bcaCreatedDate = Nothing
    , _bcaName = Nothing
    , _bcaType = Nothing
    , _bcaDescription = Nothing
    }


-- | If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to create the association.
bcaFailureReason :: Lens' BotChannelAssociation (Maybe Text)
bcaFailureReason = lens _bcaFailureReason (\ s a -> s{_bcaFailureReason = a})

-- | The status of the bot channel.      * @CREATED@ - The channel has been created and is ready for use.     * @IN_PROGRESS@ - Channel creation is in progress.     * @FAILED@ - There was an error creating the channel. For information about the reason for the failure, see the @failureReason@ field.
bcaStatus :: Lens' BotChannelAssociation (Maybe ChannelStatus)
bcaStatus = lens _bcaStatus (\ s a -> s{_bcaStatus = a})

-- | An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
bcaBotAlias :: Lens' BotChannelAssociation (Maybe Text)
bcaBotAlias = lens _bcaBotAlias (\ s a -> s{_bcaBotAlias = a})

-- | The name of the Amazon Lex bot to which this association is being made.
bcaBotName :: Lens' BotChannelAssociation (Maybe Text)
bcaBotName = lens _bcaBotName (\ s a -> s{_bcaBotName = a})

-- | Provides information necessary to communicate with the messaging platform.
bcaBotConfiguration :: Lens' BotChannelAssociation (Maybe (HashMap Text Text))
bcaBotConfiguration = lens _bcaBotConfiguration (\ s a -> s{_bcaBotConfiguration = a}) . mapping (_Sensitive . _Map)

-- | The date that the association between the Amazon Lex bot and the channel was created.
bcaCreatedDate :: Lens' BotChannelAssociation (Maybe UTCTime)
bcaCreatedDate = lens _bcaCreatedDate (\ s a -> s{_bcaCreatedDate = a}) . mapping _Time

-- | The name of the association between the bot and the channel.
bcaName :: Lens' BotChannelAssociation (Maybe Text)
bcaName = lens _bcaName (\ s a -> s{_bcaName = a})

-- | Specifies the type of association by indicating the type of channel being established between the Amazon Lex bot and the external messaging platform.
bcaType :: Lens' BotChannelAssociation (Maybe ChannelType)
bcaType = lens _bcaType (\ s a -> s{_bcaType = a})

-- | A text description of the association you are creating.
bcaDescription :: Lens' BotChannelAssociation (Maybe Text)
bcaDescription = lens _bcaDescription (\ s a -> s{_bcaDescription = a})

instance FromJSON BotChannelAssociation where
        parseJSON
          = withObject "BotChannelAssociation"
              (\ x ->
                 BotChannelAssociation' <$>
                   (x .:? "failureReason") <*> (x .:? "status") <*>
                     (x .:? "botAlias")
                     <*> (x .:? "botName")
                     <*> (x .:? "botConfiguration" .!= mempty)
                     <*> (x .:? "createdDate")
                     <*> (x .:? "name")
                     <*> (x .:? "type")
                     <*> (x .:? "description"))

instance Hashable BotChannelAssociation where

instance NFData BotChannelAssociation where

-- | Provides information about a bot. .
--
--
--
-- /See:/ 'botMetadata' smart constructor.
data BotMetadata = BotMetadata'
  { _bmStatus          :: !(Maybe LexStatus)
  , _bmCreatedDate     :: !(Maybe POSIX)
  , _bmName            :: !(Maybe Text)
  , _bmVersion         :: !(Maybe Text)
  , _bmLastUpdatedDate :: !(Maybe POSIX)
  , _bmDescription     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BotMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bmStatus' - The status of the bot.
--
-- * 'bmCreatedDate' - The date that the bot was created.
--
-- * 'bmName' - The name of the bot.
--
-- * 'bmVersion' - The version of the bot. For a new bot, the version is always @> LATEST@ .
--
-- * 'bmLastUpdatedDate' - The date that the bot was updated. When you create a bot, the creation date and last updated date are the same.
--
-- * 'bmDescription' - A description of the bot.
botMetadata
    :: BotMetadata
botMetadata =
  BotMetadata'
    { _bmStatus = Nothing
    , _bmCreatedDate = Nothing
    , _bmName = Nothing
    , _bmVersion = Nothing
    , _bmLastUpdatedDate = Nothing
    , _bmDescription = Nothing
    }


-- | The status of the bot.
bmStatus :: Lens' BotMetadata (Maybe LexStatus)
bmStatus = lens _bmStatus (\ s a -> s{_bmStatus = a})

-- | The date that the bot was created.
bmCreatedDate :: Lens' BotMetadata (Maybe UTCTime)
bmCreatedDate = lens _bmCreatedDate (\ s a -> s{_bmCreatedDate = a}) . mapping _Time

-- | The name of the bot.
bmName :: Lens' BotMetadata (Maybe Text)
bmName = lens _bmName (\ s a -> s{_bmName = a})

-- | The version of the bot. For a new bot, the version is always @> LATEST@ .
bmVersion :: Lens' BotMetadata (Maybe Text)
bmVersion = lens _bmVersion (\ s a -> s{_bmVersion = a})

-- | The date that the bot was updated. When you create a bot, the creation date and last updated date are the same.
bmLastUpdatedDate :: Lens' BotMetadata (Maybe UTCTime)
bmLastUpdatedDate = lens _bmLastUpdatedDate (\ s a -> s{_bmLastUpdatedDate = a}) . mapping _Time

-- | A description of the bot.
bmDescription :: Lens' BotMetadata (Maybe Text)
bmDescription = lens _bmDescription (\ s a -> s{_bmDescription = a})

instance FromJSON BotMetadata where
        parseJSON
          = withObject "BotMetadata"
              (\ x ->
                 BotMetadata' <$>
                   (x .:? "status") <*> (x .:? "createdDate") <*>
                     (x .:? "name")
                     <*> (x .:? "version")
                     <*> (x .:? "lastUpdatedDate")
                     <*> (x .:? "description"))

instance Hashable BotMetadata where

instance NFData BotMetadata where

-- | Provides metadata for a built-in intent.
--
--
--
-- /See:/ 'builtinIntentMetadata' smart constructor.
data BuiltinIntentMetadata = BuiltinIntentMetadata'
  { _bimSignature        :: !(Maybe Text)
  , _bimSupportedLocales :: !(Maybe [Locale])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BuiltinIntentMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bimSignature' - A unique identifier for the built-in intent. To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
--
-- * 'bimSupportedLocales' - A list of identifiers for the locales that the intent supports.
builtinIntentMetadata
    :: BuiltinIntentMetadata
builtinIntentMetadata =
  BuiltinIntentMetadata'
    {_bimSignature = Nothing, _bimSupportedLocales = Nothing}


-- | A unique identifier for the built-in intent. To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
bimSignature :: Lens' BuiltinIntentMetadata (Maybe Text)
bimSignature = lens _bimSignature (\ s a -> s{_bimSignature = a})

-- | A list of identifiers for the locales that the intent supports.
bimSupportedLocales :: Lens' BuiltinIntentMetadata [Locale]
bimSupportedLocales = lens _bimSupportedLocales (\ s a -> s{_bimSupportedLocales = a}) . _Default . _Coerce

instance FromJSON BuiltinIntentMetadata where
        parseJSON
          = withObject "BuiltinIntentMetadata"
              (\ x ->
                 BuiltinIntentMetadata' <$>
                   (x .:? "signature") <*>
                     (x .:? "supportedLocales" .!= mempty))

instance Hashable BuiltinIntentMetadata where

instance NFData BuiltinIntentMetadata where

-- | Provides information about a slot used in a built-in intent.
--
--
--
-- /See:/ 'builtinIntentSlot' smart constructor.
newtype BuiltinIntentSlot = BuiltinIntentSlot'
  { _bisName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BuiltinIntentSlot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bisName' - A list of the slots defined for the intent.
builtinIntentSlot
    :: BuiltinIntentSlot
builtinIntentSlot = BuiltinIntentSlot' {_bisName = Nothing}


-- | A list of the slots defined for the intent.
bisName :: Lens' BuiltinIntentSlot (Maybe Text)
bisName = lens _bisName (\ s a -> s{_bisName = a})

instance FromJSON BuiltinIntentSlot where
        parseJSON
          = withObject "BuiltinIntentSlot"
              (\ x -> BuiltinIntentSlot' <$> (x .:? "name"))

instance Hashable BuiltinIntentSlot where

instance NFData BuiltinIntentSlot where

-- | Provides information about a built in slot type.
--
--
--
-- /See:/ 'builtinSlotTypeMetadata' smart constructor.
data BuiltinSlotTypeMetadata = BuiltinSlotTypeMetadata'
  { _bstmSignature        :: !(Maybe Text)
  , _bstmSupportedLocales :: !(Maybe [Locale])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BuiltinSlotTypeMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bstmSignature' - A unique identifier for the built-in slot type. To find the signature for a slot type, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference> in the /Alexa Skills Kit/ .
--
-- * 'bstmSupportedLocales' - A list of target locales for the slot.
builtinSlotTypeMetadata
    :: BuiltinSlotTypeMetadata
builtinSlotTypeMetadata =
  BuiltinSlotTypeMetadata'
    {_bstmSignature = Nothing, _bstmSupportedLocales = Nothing}


-- | A unique identifier for the built-in slot type. To find the signature for a slot type, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference> in the /Alexa Skills Kit/ .
bstmSignature :: Lens' BuiltinSlotTypeMetadata (Maybe Text)
bstmSignature = lens _bstmSignature (\ s a -> s{_bstmSignature = a})

-- | A list of target locales for the slot.
bstmSupportedLocales :: Lens' BuiltinSlotTypeMetadata [Locale]
bstmSupportedLocales = lens _bstmSupportedLocales (\ s a -> s{_bstmSupportedLocales = a}) . _Default . _Coerce

instance FromJSON BuiltinSlotTypeMetadata where
        parseJSON
          = withObject "BuiltinSlotTypeMetadata"
              (\ x ->
                 BuiltinSlotTypeMetadata' <$>
                   (x .:? "signature") <*>
                     (x .:? "supportedLocales" .!= mempty))

instance Hashable BuiltinSlotTypeMetadata where

instance NFData BuiltinSlotTypeMetadata where

-- | Specifies a Lambda function that verifies requests to a bot or fulfills the user's request to a bot..
--
--
--
-- /See:/ 'codeHook' smart constructor.
data CodeHook = CodeHook'
  { _chUri            :: !Text
  , _chMessageVersion :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CodeHook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chUri' - The Amazon Resource Name (ARN) of the Lambda function.
--
-- * 'chMessageVersion' - The version of the request-response that you want Amazon Lex to use to invoke your Lambda function. For more information, see 'using-lambda' .
codeHook
    :: Text -- ^ 'chUri'
    -> Text -- ^ 'chMessageVersion'
    -> CodeHook
codeHook pUri_ pMessageVersion_ =
  CodeHook' {_chUri = pUri_, _chMessageVersion = pMessageVersion_}


-- | The Amazon Resource Name (ARN) of the Lambda function.
chUri :: Lens' CodeHook Text
chUri = lens _chUri (\ s a -> s{_chUri = a})

-- | The version of the request-response that you want Amazon Lex to use to invoke your Lambda function. For more information, see 'using-lambda' .
chMessageVersion :: Lens' CodeHook Text
chMessageVersion = lens _chMessageVersion (\ s a -> s{_chMessageVersion = a})

instance FromJSON CodeHook where
        parseJSON
          = withObject "CodeHook"
              (\ x ->
                 CodeHook' <$>
                   (x .: "uri") <*> (x .: "messageVersion"))

instance Hashable CodeHook where

instance NFData CodeHook where

instance ToJSON CodeHook where
        toJSON CodeHook'{..}
          = object
              (catMaybes
                 [Just ("uri" .= _chUri),
                  Just ("messageVersion" .= _chMessageVersion)])

-- | Each slot type can have a set of values. Each enumeration value represents a value the slot type can take.
--
--
-- For example, a pizza ordering bot could have a slot type that specifies the type of crust that the pizza should have. The slot type could include the values
--
--     * thick
--
--     * thin
--
--     * stuffed
--
--
--
--
-- /See:/ 'enumerationValue' smart constructor.
data EnumerationValue = EnumerationValue'
  { _evSynonyms :: !(Maybe [Text])
  , _evValue    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnumerationValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'evSynonyms' - Additional values related to the slot type value.
--
-- * 'evValue' - The value of the slot type.
enumerationValue
    :: Text -- ^ 'evValue'
    -> EnumerationValue
enumerationValue pValue_ =
  EnumerationValue' {_evSynonyms = Nothing, _evValue = pValue_}


-- | Additional values related to the slot type value.
evSynonyms :: Lens' EnumerationValue [Text]
evSynonyms = lens _evSynonyms (\ s a -> s{_evSynonyms = a}) . _Default . _Coerce

-- | The value of the slot type.
evValue :: Lens' EnumerationValue Text
evValue = lens _evValue (\ s a -> s{_evValue = a})

instance FromJSON EnumerationValue where
        parseJSON
          = withObject "EnumerationValue"
              (\ x ->
                 EnumerationValue' <$>
                   (x .:? "synonyms" .!= mempty) <*> (x .: "value"))

instance Hashable EnumerationValue where

instance NFData EnumerationValue where

instance ToJSON EnumerationValue where
        toJSON EnumerationValue'{..}
          = object
              (catMaybes
                 [("synonyms" .=) <$> _evSynonyms,
                  Just ("value" .= _evValue)])

-- | A prompt for additional activity after an intent is fulfilled. For example, after the @OrderPizza@ intent is fulfilled, you might prompt the user to find out whether the user wants to order drinks.
--
--
--
-- /See:/ 'followUpPrompt' smart constructor.
data FollowUpPrompt = FollowUpPrompt'
  { _fupPrompt             :: !Prompt
  , _fupRejectionStatement :: !Statement
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FollowUpPrompt' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fupPrompt' - Prompts for information from the user.
--
-- * 'fupRejectionStatement' - If the user answers "no" to the question defined in the @prompt@ field, Amazon Lex responds with this statement to acknowledge that the intent was canceled.
followUpPrompt
    :: Prompt -- ^ 'fupPrompt'
    -> Statement -- ^ 'fupRejectionStatement'
    -> FollowUpPrompt
followUpPrompt pPrompt_ pRejectionStatement_ =
  FollowUpPrompt'
    {_fupPrompt = pPrompt_, _fupRejectionStatement = pRejectionStatement_}


-- | Prompts for information from the user.
fupPrompt :: Lens' FollowUpPrompt Prompt
fupPrompt = lens _fupPrompt (\ s a -> s{_fupPrompt = a})

-- | If the user answers "no" to the question defined in the @prompt@ field, Amazon Lex responds with this statement to acknowledge that the intent was canceled.
fupRejectionStatement :: Lens' FollowUpPrompt Statement
fupRejectionStatement = lens _fupRejectionStatement (\ s a -> s{_fupRejectionStatement = a})

instance FromJSON FollowUpPrompt where
        parseJSON
          = withObject "FollowUpPrompt"
              (\ x ->
                 FollowUpPrompt' <$>
                   (x .: "prompt") <*> (x .: "rejectionStatement"))

instance Hashable FollowUpPrompt where

instance NFData FollowUpPrompt where

instance ToJSON FollowUpPrompt where
        toJSON FollowUpPrompt'{..}
          = object
              (catMaybes
                 [Just ("prompt" .= _fupPrompt),
                  Just
                    ("rejectionStatement" .= _fupRejectionStatement)])

-- | Describes how the intent is fulfilled after the user provides all of the information required for the intent. You can provide a Lambda function to process the intent, or you can return the intent information to the client application. We recommend that you use a Lambda function so that the relevant logic lives in the Cloud and limit the client-side code primarily to presentation. If you need to update the logic, you only update the Lambda function; you don't need to upgrade your client application.
--
--
-- Consider the following examples:
--
--     * In a pizza ordering application, after the user provides all of the information for placing an order, you use a Lambda function to place an order with a pizzeria.
--
--     * In a gaming application, when a user says "pick up a rock," this information must go back to the client application so that it can perform the operation and update the graphics. In this case, you want Amazon Lex to return the intent data to the client.
--
--
--
--
-- /See:/ 'fulfillmentActivity' smart constructor.
data FulfillmentActivity = FulfillmentActivity'
  { _faCodeHook :: !(Maybe CodeHook)
  , _faType     :: !FulfillmentActivityType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FulfillmentActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'faCodeHook' - A description of the Lambda function that is run to fulfill the intent.
--
-- * 'faType' - How the intent should be fulfilled, either by running a Lambda function or by returning the slot data to the client application.
fulfillmentActivity
    :: FulfillmentActivityType -- ^ 'faType'
    -> FulfillmentActivity
fulfillmentActivity pType_ =
  FulfillmentActivity' {_faCodeHook = Nothing, _faType = pType_}


-- | A description of the Lambda function that is run to fulfill the intent.
faCodeHook :: Lens' FulfillmentActivity (Maybe CodeHook)
faCodeHook = lens _faCodeHook (\ s a -> s{_faCodeHook = a})

-- | How the intent should be fulfilled, either by running a Lambda function or by returning the slot data to the client application.
faType :: Lens' FulfillmentActivity FulfillmentActivityType
faType = lens _faType (\ s a -> s{_faType = a})

instance FromJSON FulfillmentActivity where
        parseJSON
          = withObject "FulfillmentActivity"
              (\ x ->
                 FulfillmentActivity' <$>
                   (x .:? "codeHook") <*> (x .: "type"))

instance Hashable FulfillmentActivity where

instance NFData FulfillmentActivity where

instance ToJSON FulfillmentActivity where
        toJSON FulfillmentActivity'{..}
          = object
              (catMaybes
                 [("codeHook" .=) <$> _faCodeHook,
                  Just ("type" .= _faType)])

-- | Identifies the specific version of an intent.
--
--
--
-- /See:/ 'intent' smart constructor.
data Intent = Intent'
  { _iIntentName    :: !Text
  , _iIntentVersion :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Intent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iIntentName' - The name of the intent.
--
-- * 'iIntentVersion' - The version of the intent.
intent
    :: Text -- ^ 'iIntentName'
    -> Text -- ^ 'iIntentVersion'
    -> Intent
intent pIntentName_ pIntentVersion_ =
  Intent' {_iIntentName = pIntentName_, _iIntentVersion = pIntentVersion_}


-- | The name of the intent.
iIntentName :: Lens' Intent Text
iIntentName = lens _iIntentName (\ s a -> s{_iIntentName = a})

-- | The version of the intent.
iIntentVersion :: Lens' Intent Text
iIntentVersion = lens _iIntentVersion (\ s a -> s{_iIntentVersion = a})

instance FromJSON Intent where
        parseJSON
          = withObject "Intent"
              (\ x ->
                 Intent' <$>
                   (x .: "intentName") <*> (x .: "intentVersion"))

instance Hashable Intent where

instance NFData Intent where

instance ToJSON Intent where
        toJSON Intent'{..}
          = object
              (catMaybes
                 [Just ("intentName" .= _iIntentName),
                  Just ("intentVersion" .= _iIntentVersion)])

-- | Provides information about an intent.
--
--
--
-- /See:/ 'intentMetadata' smart constructor.
data IntentMetadata = IntentMetadata'
  { _imCreatedDate     :: !(Maybe POSIX)
  , _imName            :: !(Maybe Text)
  , _imVersion         :: !(Maybe Text)
  , _imLastUpdatedDate :: !(Maybe POSIX)
  , _imDescription     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IntentMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imCreatedDate' - The date that the intent was created.
--
-- * 'imName' - The name of the intent.
--
-- * 'imVersion' - The version of the intent.
--
-- * 'imLastUpdatedDate' - The date that the intent was updated. When you create an intent, the creation date and last updated date are the same.
--
-- * 'imDescription' - A description of the intent.
intentMetadata
    :: IntentMetadata
intentMetadata =
  IntentMetadata'
    { _imCreatedDate = Nothing
    , _imName = Nothing
    , _imVersion = Nothing
    , _imLastUpdatedDate = Nothing
    , _imDescription = Nothing
    }


-- | The date that the intent was created.
imCreatedDate :: Lens' IntentMetadata (Maybe UTCTime)
imCreatedDate = lens _imCreatedDate (\ s a -> s{_imCreatedDate = a}) . mapping _Time

-- | The name of the intent.
imName :: Lens' IntentMetadata (Maybe Text)
imName = lens _imName (\ s a -> s{_imName = a})

-- | The version of the intent.
imVersion :: Lens' IntentMetadata (Maybe Text)
imVersion = lens _imVersion (\ s a -> s{_imVersion = a})

-- | The date that the intent was updated. When you create an intent, the creation date and last updated date are the same.
imLastUpdatedDate :: Lens' IntentMetadata (Maybe UTCTime)
imLastUpdatedDate = lens _imLastUpdatedDate (\ s a -> s{_imLastUpdatedDate = a}) . mapping _Time

-- | A description of the intent.
imDescription :: Lens' IntentMetadata (Maybe Text)
imDescription = lens _imDescription (\ s a -> s{_imDescription = a})

instance FromJSON IntentMetadata where
        parseJSON
          = withObject "IntentMetadata"
              (\ x ->
                 IntentMetadata' <$>
                   (x .:? "createdDate") <*> (x .:? "name") <*>
                     (x .:? "version")
                     <*> (x .:? "lastUpdatedDate")
                     <*> (x .:? "description"))

instance Hashable IntentMetadata where

instance NFData IntentMetadata where

-- | The message object that provides the message text and its type.
--
--
--
-- /See:/ 'message' smart constructor.
data Message = Message'
  { _mGroupNumber :: !(Maybe Nat)
  , _mContentType :: !ContentType
  , _mContent     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Message' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mGroupNumber' - Identifies the message group that the message belongs to. When a group is assigned to a message, Amazon Lex returns one message from each group in the response.
--
-- * 'mContentType' - The content type of the message string.
--
-- * 'mContent' - The text of the message.
message
    :: ContentType -- ^ 'mContentType'
    -> Text -- ^ 'mContent'
    -> Message
message pContentType_ pContent_ =
  Message'
    { _mGroupNumber = Nothing
    , _mContentType = pContentType_
    , _mContent = pContent_
    }


-- | Identifies the message group that the message belongs to. When a group is assigned to a message, Amazon Lex returns one message from each group in the response.
mGroupNumber :: Lens' Message (Maybe Natural)
mGroupNumber = lens _mGroupNumber (\ s a -> s{_mGroupNumber = a}) . mapping _Nat

-- | The content type of the message string.
mContentType :: Lens' Message ContentType
mContentType = lens _mContentType (\ s a -> s{_mContentType = a})

-- | The text of the message.
mContent :: Lens' Message Text
mContent = lens _mContent (\ s a -> s{_mContent = a})

instance FromJSON Message where
        parseJSON
          = withObject "Message"
              (\ x ->
                 Message' <$>
                   (x .:? "groupNumber") <*> (x .: "contentType") <*>
                     (x .: "content"))

instance Hashable Message where

instance NFData Message where

instance ToJSON Message where
        toJSON Message'{..}
          = object
              (catMaybes
                 [("groupNumber" .=) <$> _mGroupNumber,
                  Just ("contentType" .= _mContentType),
                  Just ("content" .= _mContent)])

-- | Obtains information from the user. To define a prompt, provide one or more messages and specify the number of attempts to get information from the user. If you provide more than one message, Amazon Lex chooses one of the messages to use to prompt the user. For more information, see 'how-it-works' .
--
--
--
-- /See:/ 'prompt' smart constructor.
data Prompt = Prompt'
  { _pResponseCard :: !(Maybe Text)
  , _pMessages     :: !(List1 Message)
  , _pMaxAttempts  :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Prompt' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pResponseCard' - A response card. Amazon Lex uses this prompt at runtime, in the @PostText@ API response. It substitutes session attributes and slot values for placeholders in the response card. For more information, see 'ex-resp-card' .
--
-- * 'pMessages' - An array of objects, each of which provides a message string and its type. You can specify the message string in plain text or in Speech Synthesis Markup Language (SSML).
--
-- * 'pMaxAttempts' - The number of times to prompt the user for information.
prompt
    :: NonEmpty Message -- ^ 'pMessages'
    -> Natural -- ^ 'pMaxAttempts'
    -> Prompt
prompt pMessages_ pMaxAttempts_ =
  Prompt'
    { _pResponseCard = Nothing
    , _pMessages = _List1 # pMessages_
    , _pMaxAttempts = _Nat # pMaxAttempts_
    }


-- | A response card. Amazon Lex uses this prompt at runtime, in the @PostText@ API response. It substitutes session attributes and slot values for placeholders in the response card. For more information, see 'ex-resp-card' .
pResponseCard :: Lens' Prompt (Maybe Text)
pResponseCard = lens _pResponseCard (\ s a -> s{_pResponseCard = a})

-- | An array of objects, each of which provides a message string and its type. You can specify the message string in plain text or in Speech Synthesis Markup Language (SSML).
pMessages :: Lens' Prompt (NonEmpty Message)
pMessages = lens _pMessages (\ s a -> s{_pMessages = a}) . _List1

-- | The number of times to prompt the user for information.
pMaxAttempts :: Lens' Prompt Natural
pMaxAttempts = lens _pMaxAttempts (\ s a -> s{_pMaxAttempts = a}) . _Nat

instance FromJSON Prompt where
        parseJSON
          = withObject "Prompt"
              (\ x ->
                 Prompt' <$>
                   (x .:? "responseCard") <*> (x .: "messages") <*>
                     (x .: "maxAttempts"))

instance Hashable Prompt where

instance NFData Prompt where

instance ToJSON Prompt where
        toJSON Prompt'{..}
          = object
              (catMaybes
                 [("responseCard" .=) <$> _pResponseCard,
                  Just ("messages" .= _pMessages),
                  Just ("maxAttempts" .= _pMaxAttempts)])

-- | Identifies the version of a specific slot.
--
--
--
-- /See:/ 'slot' smart constructor.
data Slot = Slot'
  { _sSlotType               :: !(Maybe Text)
  , _sValueElicitationPrompt :: !(Maybe Prompt)
  , _sResponseCard           :: !(Maybe Text)
  , _sPriority               :: !(Maybe Nat)
  , _sSlotTypeVersion        :: !(Maybe Text)
  , _sSampleUtterances       :: !(Maybe [Text])
  , _sDescription            :: !(Maybe Text)
  , _sName                   :: !Text
  , _sSlotConstraint         :: !SlotConstraint
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
-- * 'sPriority' - Directs Lex the order in which to elicit this slot value from the user. For example, if the intent has two slots with priorities 1 and 2, AWS Lex first elicits a value for the slot with priority 1. If multiple slots share the same priority, the order in which Lex elicits values is arbitrary.
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
slot
    :: Text -- ^ 'sName'
    -> SlotConstraint -- ^ 'sSlotConstraint'
    -> Slot
slot pName_ pSlotConstraint_ =
  Slot'
    { _sSlotType = Nothing
    , _sValueElicitationPrompt = Nothing
    , _sResponseCard = Nothing
    , _sPriority = Nothing
    , _sSlotTypeVersion = Nothing
    , _sSampleUtterances = Nothing
    , _sDescription = Nothing
    , _sName = pName_
    , _sSlotConstraint = pSlotConstraint_
    }


-- | The type of the slot, either a custom slot type that you defined or one of the built-in slot types.
sSlotType :: Lens' Slot (Maybe Text)
sSlotType = lens _sSlotType (\ s a -> s{_sSlotType = a})

-- | The prompt that Amazon Lex uses to elicit the slot value from the user.
sValueElicitationPrompt :: Lens' Slot (Maybe Prompt)
sValueElicitationPrompt = lens _sValueElicitationPrompt (\ s a -> s{_sValueElicitationPrompt = a})

-- | A set of possible responses for the slot type used by text-based clients. A user chooses an option from the response card, instead of using text to reply.
sResponseCard :: Lens' Slot (Maybe Text)
sResponseCard = lens _sResponseCard (\ s a -> s{_sResponseCard = a})

-- | Directs Lex the order in which to elicit this slot value from the user. For example, if the intent has two slots with priorities 1 and 2, AWS Lex first elicits a value for the slot with priority 1. If multiple slots share the same priority, the order in which Lex elicits values is arbitrary.
sPriority :: Lens' Slot (Maybe Natural)
sPriority = lens _sPriority (\ s a -> s{_sPriority = a}) . mapping _Nat

-- | The version of the slot type.
sSlotTypeVersion :: Lens' Slot (Maybe Text)
sSlotTypeVersion = lens _sSlotTypeVersion (\ s a -> s{_sSlotTypeVersion = a})

-- | If you know a specific pattern with which users might respond to an Amazon Lex request for a slot value, you can provide those utterances to improve accuracy. This is optional. In most cases, Amazon Lex is capable of understanding user utterances.
sSampleUtterances :: Lens' Slot [Text]
sSampleUtterances = lens _sSampleUtterances (\ s a -> s{_sSampleUtterances = a}) . _Default . _Coerce

-- | A description of the slot.
sDescription :: Lens' Slot (Maybe Text)
sDescription = lens _sDescription (\ s a -> s{_sDescription = a})

-- | The name of the slot.
sName :: Lens' Slot Text
sName = lens _sName (\ s a -> s{_sName = a})

-- | Specifies whether the slot is required or optional.
sSlotConstraint :: Lens' Slot SlotConstraint
sSlotConstraint = lens _sSlotConstraint (\ s a -> s{_sSlotConstraint = a})

instance FromJSON Slot where
        parseJSON
          = withObject "Slot"
              (\ x ->
                 Slot' <$>
                   (x .:? "slotType") <*>
                     (x .:? "valueElicitationPrompt")
                     <*> (x .:? "responseCard")
                     <*> (x .:? "priority")
                     <*> (x .:? "slotTypeVersion")
                     <*> (x .:? "sampleUtterances" .!= mempty)
                     <*> (x .:? "description")
                     <*> (x .: "name")
                     <*> (x .: "slotConstraint"))

instance Hashable Slot where

instance NFData Slot where

instance ToJSON Slot where
        toJSON Slot'{..}
          = object
              (catMaybes
                 [("slotType" .=) <$> _sSlotType,
                  ("valueElicitationPrompt" .=) <$>
                    _sValueElicitationPrompt,
                  ("responseCard" .=) <$> _sResponseCard,
                  ("priority" .=) <$> _sPriority,
                  ("slotTypeVersion" .=) <$> _sSlotTypeVersion,
                  ("sampleUtterances" .=) <$> _sSampleUtterances,
                  ("description" .=) <$> _sDescription,
                  Just ("name" .= _sName),
                  Just ("slotConstraint" .= _sSlotConstraint)])

-- | Provides information about a slot type..
--
--
--
-- /See:/ 'slotTypeMetadata' smart constructor.
data SlotTypeMetadata = SlotTypeMetadata'
  { _stmCreatedDate     :: !(Maybe POSIX)
  , _stmName            :: !(Maybe Text)
  , _stmVersion         :: !(Maybe Text)
  , _stmLastUpdatedDate :: !(Maybe POSIX)
  , _stmDescription     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SlotTypeMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stmCreatedDate' - The date that the slot type was created.
--
-- * 'stmName' - The name of the slot type.
--
-- * 'stmVersion' - The version of the slot type.
--
-- * 'stmLastUpdatedDate' - The date that the slot type was updated. When you create a resource, the creation date and last updated date are the same.
--
-- * 'stmDescription' - A description of the slot type.
slotTypeMetadata
    :: SlotTypeMetadata
slotTypeMetadata =
  SlotTypeMetadata'
    { _stmCreatedDate = Nothing
    , _stmName = Nothing
    , _stmVersion = Nothing
    , _stmLastUpdatedDate = Nothing
    , _stmDescription = Nothing
    }


-- | The date that the slot type was created.
stmCreatedDate :: Lens' SlotTypeMetadata (Maybe UTCTime)
stmCreatedDate = lens _stmCreatedDate (\ s a -> s{_stmCreatedDate = a}) . mapping _Time

-- | The name of the slot type.
stmName :: Lens' SlotTypeMetadata (Maybe Text)
stmName = lens _stmName (\ s a -> s{_stmName = a})

-- | The version of the slot type.
stmVersion :: Lens' SlotTypeMetadata (Maybe Text)
stmVersion = lens _stmVersion (\ s a -> s{_stmVersion = a})

-- | The date that the slot type was updated. When you create a resource, the creation date and last updated date are the same.
stmLastUpdatedDate :: Lens' SlotTypeMetadata (Maybe UTCTime)
stmLastUpdatedDate = lens _stmLastUpdatedDate (\ s a -> s{_stmLastUpdatedDate = a}) . mapping _Time

-- | A description of the slot type.
stmDescription :: Lens' SlotTypeMetadata (Maybe Text)
stmDescription = lens _stmDescription (\ s a -> s{_stmDescription = a})

instance FromJSON SlotTypeMetadata where
        parseJSON
          = withObject "SlotTypeMetadata"
              (\ x ->
                 SlotTypeMetadata' <$>
                   (x .:? "createdDate") <*> (x .:? "name") <*>
                     (x .:? "version")
                     <*> (x .:? "lastUpdatedDate")
                     <*> (x .:? "description"))

instance Hashable SlotTypeMetadata where

instance NFData SlotTypeMetadata where

-- | A collection of messages that convey information to the user. At runtime, Amazon Lex selects the message to convey.
--
--
--
-- /See:/ 'statement' smart constructor.
data Statement = Statement'
  { _staResponseCard :: !(Maybe Text)
  , _staMessages     :: !(List1 Message)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Statement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'staResponseCard' - At runtime, if the client is using the <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> API, Amazon Lex includes the response card in the response. It substitutes all of the session attributes and slot values for placeholders in the response card.
--
-- * 'staMessages' - A collection of message objects.
statement
    :: NonEmpty Message -- ^ 'staMessages'
    -> Statement
statement pMessages_ =
  Statement' {_staResponseCard = Nothing, _staMessages = _List1 # pMessages_}


-- | At runtime, if the client is using the <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> API, Amazon Lex includes the response card in the response. It substitutes all of the session attributes and slot values for placeholders in the response card.
staResponseCard :: Lens' Statement (Maybe Text)
staResponseCard = lens _staResponseCard (\ s a -> s{_staResponseCard = a})

-- | A collection of message objects.
staMessages :: Lens' Statement (NonEmpty Message)
staMessages = lens _staMessages (\ s a -> s{_staMessages = a}) . _List1

instance FromJSON Statement where
        parseJSON
          = withObject "Statement"
              (\ x ->
                 Statement' <$>
                   (x .:? "responseCard") <*> (x .: "messages"))

instance Hashable Statement where

instance NFData Statement where

instance ToJSON Statement where
        toJSON Statement'{..}
          = object
              (catMaybes
                 [("responseCard" .=) <$> _staResponseCard,
                  Just ("messages" .= _staMessages)])

-- | Provides information about a single utterance that was made to your bot.
--
--
--
-- /See:/ 'utteranceData' smart constructor.
data UtteranceData = UtteranceData'
  { _udFirstUtteredDate :: !(Maybe POSIX)
  , _udCount            :: !(Maybe Int)
  , _udUtteranceString  :: !(Maybe Text)
  , _udLastUtteredDate  :: !(Maybe POSIX)
  , _udDistinctUsers    :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UtteranceData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udFirstUtteredDate' - The date that the utterance was first recorded.
--
-- * 'udCount' - The number of times that the utterance was processed.
--
-- * 'udUtteranceString' - The text that was entered by the user or the text representation of an audio clip.
--
-- * 'udLastUtteredDate' - The date that the utterance was last recorded.
--
-- * 'udDistinctUsers' - The total number of individuals that used the utterance.
utteranceData
    :: UtteranceData
utteranceData =
  UtteranceData'
    { _udFirstUtteredDate = Nothing
    , _udCount = Nothing
    , _udUtteranceString = Nothing
    , _udLastUtteredDate = Nothing
    , _udDistinctUsers = Nothing
    }


-- | The date that the utterance was first recorded.
udFirstUtteredDate :: Lens' UtteranceData (Maybe UTCTime)
udFirstUtteredDate = lens _udFirstUtteredDate (\ s a -> s{_udFirstUtteredDate = a}) . mapping _Time

-- | The number of times that the utterance was processed.
udCount :: Lens' UtteranceData (Maybe Int)
udCount = lens _udCount (\ s a -> s{_udCount = a})

-- | The text that was entered by the user or the text representation of an audio clip.
udUtteranceString :: Lens' UtteranceData (Maybe Text)
udUtteranceString = lens _udUtteranceString (\ s a -> s{_udUtteranceString = a})

-- | The date that the utterance was last recorded.
udLastUtteredDate :: Lens' UtteranceData (Maybe UTCTime)
udLastUtteredDate = lens _udLastUtteredDate (\ s a -> s{_udLastUtteredDate = a}) . mapping _Time

-- | The total number of individuals that used the utterance.
udDistinctUsers :: Lens' UtteranceData (Maybe Int)
udDistinctUsers = lens _udDistinctUsers (\ s a -> s{_udDistinctUsers = a})

instance FromJSON UtteranceData where
        parseJSON
          = withObject "UtteranceData"
              (\ x ->
                 UtteranceData' <$>
                   (x .:? "firstUtteredDate") <*> (x .:? "count") <*>
                     (x .:? "utteranceString")
                     <*> (x .:? "lastUtteredDate")
                     <*> (x .:? "distinctUsers"))

instance Hashable UtteranceData where

instance NFData UtteranceData where

-- | Provides a list of utterances that have been made to a specific version of your bot. The list contains a maximum of 100 utterances.
--
--
--
-- /See:/ 'utteranceList' smart constructor.
data UtteranceList = UtteranceList'
  { _ulBotVersion :: !(Maybe Text)
  , _ulUtterances :: !(Maybe [UtteranceData])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UtteranceList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ulBotVersion' - The version of the bot that processed the list.
--
-- * 'ulUtterances' - One or more 'UtteranceData' objects that contain information about the utterances that have been made to a bot. The maximum number of object is 100.
utteranceList
    :: UtteranceList
utteranceList =
  UtteranceList' {_ulBotVersion = Nothing, _ulUtterances = Nothing}


-- | The version of the bot that processed the list.
ulBotVersion :: Lens' UtteranceList (Maybe Text)
ulBotVersion = lens _ulBotVersion (\ s a -> s{_ulBotVersion = a})

-- | One or more 'UtteranceData' objects that contain information about the utterances that have been made to a bot. The maximum number of object is 100.
ulUtterances :: Lens' UtteranceList [UtteranceData]
ulUtterances = lens _ulUtterances (\ s a -> s{_ulUtterances = a}) . _Default . _Coerce

instance FromJSON UtteranceList where
        parseJSON
          = withObject "UtteranceList"
              (\ x ->
                 UtteranceList' <$>
                   (x .:? "botVersion") <*>
                     (x .:? "utterances" .!= mempty))

instance Hashable UtteranceList where

instance NFData UtteranceList where

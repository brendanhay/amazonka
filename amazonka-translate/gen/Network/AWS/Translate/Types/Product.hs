{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Translate.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Translate.Types.Sum

-- | The custom terminology applied to the input text by Amazon Translate for the translated text response. This is optional in the response and will only be present if you specified terminology input in the request. Currently, only one terminology can be applied per TranslateText request.
--
--
--
-- /See:/ 'appliedTerminology' smart constructor.
data AppliedTerminology = AppliedTerminology'
  { _atTerms :: !(Maybe [Term])
  , _atName  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AppliedTerminology' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atTerms' - The specific terms of the custom terminology applied to the input text by Amazon Translate for the translated text response. A maximum of 250 terms will be returned, and the specific terms applied will be the first 250 terms in the source text.
--
-- * 'atName' - The name of the custom terminology applied to the input text by Amazon Translate for the translated text response.
appliedTerminology
    :: AppliedTerminology
appliedTerminology = AppliedTerminology' {_atTerms = Nothing, _atName = Nothing}


-- | The specific terms of the custom terminology applied to the input text by Amazon Translate for the translated text response. A maximum of 250 terms will be returned, and the specific terms applied will be the first 250 terms in the source text.
atTerms :: Lens' AppliedTerminology [Term]
atTerms = lens _atTerms (\ s a -> s{_atTerms = a}) . _Default . _Coerce

-- | The name of the custom terminology applied to the input text by Amazon Translate for the translated text response.
atName :: Lens' AppliedTerminology (Maybe Text)
atName = lens _atName (\ s a -> s{_atName = a})

instance FromJSON AppliedTerminology where
        parseJSON
          = withObject "AppliedTerminology"
              (\ x ->
                 AppliedTerminology' <$>
                   (x .:? "Terms" .!= mempty) <*> (x .:? "Name"))

instance Hashable AppliedTerminology where

instance NFData AppliedTerminology where

-- | The encryption key used to encrypt the custom terminologies used by Amazon Translate.
--
--
--
-- /See:/ 'encryptionKey' smart constructor.
data EncryptionKey = EncryptionKey'
  { _ekType :: !EncryptionKeyType
  , _ekId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EncryptionKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ekType' - The type of encryption key used by Amazon Translate to encrypt custom terminologies.
--
-- * 'ekId' - The Amazon Resource Name (ARN) of the encryption key being used to encrypt the custom terminology.
encryptionKey
    :: EncryptionKeyType -- ^ 'ekType'
    -> Text -- ^ 'ekId'
    -> EncryptionKey
encryptionKey pType_ pId_ = EncryptionKey' {_ekType = pType_, _ekId = pId_}


-- | The type of encryption key used by Amazon Translate to encrypt custom terminologies.
ekType :: Lens' EncryptionKey EncryptionKeyType
ekType = lens _ekType (\ s a -> s{_ekType = a})

-- | The Amazon Resource Name (ARN) of the encryption key being used to encrypt the custom terminology.
ekId :: Lens' EncryptionKey Text
ekId = lens _ekId (\ s a -> s{_ekId = a})

instance FromJSON EncryptionKey where
        parseJSON
          = withObject "EncryptionKey"
              (\ x ->
                 EncryptionKey' <$> (x .: "Type") <*> (x .: "Id"))

instance Hashable EncryptionKey where

instance NFData EncryptionKey where

instance ToJSON EncryptionKey where
        toJSON EncryptionKey'{..}
          = object
              (catMaybes
                 [Just ("Type" .= _ekType), Just ("Id" .= _ekId)])

-- | The term being translated by the custom terminology.
--
--
--
-- /See:/ 'term' smart constructor.
data Term = Term'
  { _tTargetText :: !(Maybe Text)
  , _tSourceText :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Term' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tTargetText' - The target text of the term being translated by the custom terminology.
--
-- * 'tSourceText' - The source text of the term being translated by the custom terminology.
term
    :: Term
term = Term' {_tTargetText = Nothing, _tSourceText = Nothing}


-- | The target text of the term being translated by the custom terminology.
tTargetText :: Lens' Term (Maybe Text)
tTargetText = lens _tTargetText (\ s a -> s{_tTargetText = a})

-- | The source text of the term being translated by the custom terminology.
tSourceText :: Lens' Term (Maybe Text)
tSourceText = lens _tSourceText (\ s a -> s{_tSourceText = a})

instance FromJSON Term where
        parseJSON
          = withObject "Term"
              (\ x ->
                 Term' <$>
                   (x .:? "TargetText") <*> (x .:? "SourceText"))

instance Hashable Term where

instance NFData Term where

-- | The data associated with the custom terminology.
--
--
--
-- /See:/ 'terminologyData' smart constructor.
data TerminologyData = TerminologyData'
  { _tdFile   :: !(Sensitive Base64)
  , _tdFormat :: !TerminologyDataFormat
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'TerminologyData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdFile' - The file containing the custom terminology data.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'tdFormat' - The data format of the custom terminology. Either CSV or TMX.
terminologyData
    :: ByteString -- ^ 'tdFile'
    -> TerminologyDataFormat -- ^ 'tdFormat'
    -> TerminologyData
terminologyData pFile_ pFormat_ =
  TerminologyData'
    {_tdFile = _Sensitive . _Base64 # pFile_, _tdFormat = pFormat_}


-- | The file containing the custom terminology data.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
tdFile :: Lens' TerminologyData ByteString
tdFile = lens _tdFile (\ s a -> s{_tdFile = a}) . _Sensitive . _Base64

-- | The data format of the custom terminology. Either CSV or TMX.
tdFormat :: Lens' TerminologyData TerminologyDataFormat
tdFormat = lens _tdFormat (\ s a -> s{_tdFormat = a})

instance Hashable TerminologyData where

instance NFData TerminologyData where

instance ToJSON TerminologyData where
        toJSON TerminologyData'{..}
          = object
              (catMaybes
                 [Just ("File" .= _tdFile),
                  Just ("Format" .= _tdFormat)])

-- | The location of the custom terminology data.
--
--
--
-- /See:/ 'terminologyDataLocation' smart constructor.
data TerminologyDataLocation = TerminologyDataLocation'
  { _tdlRepositoryType :: !Text
  , _tdlLocation       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TerminologyDataLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdlRepositoryType' - The repository type for the custom terminology data.
--
-- * 'tdlLocation' - The location of the custom terminology data.
terminologyDataLocation
    :: Text -- ^ 'tdlRepositoryType'
    -> Text -- ^ 'tdlLocation'
    -> TerminologyDataLocation
terminologyDataLocation pRepositoryType_ pLocation_ =
  TerminologyDataLocation'
    {_tdlRepositoryType = pRepositoryType_, _tdlLocation = pLocation_}


-- | The repository type for the custom terminology data.
tdlRepositoryType :: Lens' TerminologyDataLocation Text
tdlRepositoryType = lens _tdlRepositoryType (\ s a -> s{_tdlRepositoryType = a})

-- | The location of the custom terminology data.
tdlLocation :: Lens' TerminologyDataLocation Text
tdlLocation = lens _tdlLocation (\ s a -> s{_tdlLocation = a})

instance FromJSON TerminologyDataLocation where
        parseJSON
          = withObject "TerminologyDataLocation"
              (\ x ->
                 TerminologyDataLocation' <$>
                   (x .: "RepositoryType") <*> (x .: "Location"))

instance Hashable TerminologyDataLocation where

instance NFData TerminologyDataLocation where

-- | The properties of the custom terminology.
--
--
--
-- /See:/ 'terminologyProperties' smart constructor.
data TerminologyProperties = TerminologyProperties'
  { _tpSizeBytes           :: !(Maybe Int)
  , _tpLastUpdatedAt       :: !(Maybe POSIX)
  , _tpARN                 :: !(Maybe Text)
  , _tpTargetLanguageCodes :: !(Maybe [Text])
  , _tpCreatedAt           :: !(Maybe POSIX)
  , _tpName                :: !(Maybe Text)
  , _tpSourceLanguageCode  :: !(Maybe Text)
  , _tpTermCount           :: !(Maybe Int)
  , _tpEncryptionKey       :: !(Maybe EncryptionKey)
  , _tpDescription         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TerminologyProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpSizeBytes' - The size of the file used when importing a custom terminology.
--
-- * 'tpLastUpdatedAt' - The time at which the custom terminology was last update, based on the timestamp.
--
-- * 'tpARN' - The Amazon Resource Name (ARN) of the custom terminology.
--
-- * 'tpTargetLanguageCodes' - The language codes for the target languages available with the custom terminology file. All possible target languages are returned in array.
--
-- * 'tpCreatedAt' - The time at which the custom terminology was created, based on the timestamp.
--
-- * 'tpName' - The name of the custom terminology.
--
-- * 'tpSourceLanguageCode' - The language code for the source text of the translation request for which the custom terminology is being used.
--
-- * 'tpTermCount' - The number of terms included in the custom terminology.
--
-- * 'tpEncryptionKey' - The encryption key for the custom terminology.
--
-- * 'tpDescription' - The description of the custom terminology properties.
terminologyProperties
    :: TerminologyProperties
terminologyProperties =
  TerminologyProperties'
    { _tpSizeBytes = Nothing
    , _tpLastUpdatedAt = Nothing
    , _tpARN = Nothing
    , _tpTargetLanguageCodes = Nothing
    , _tpCreatedAt = Nothing
    , _tpName = Nothing
    , _tpSourceLanguageCode = Nothing
    , _tpTermCount = Nothing
    , _tpEncryptionKey = Nothing
    , _tpDescription = Nothing
    }


-- | The size of the file used when importing a custom terminology.
tpSizeBytes :: Lens' TerminologyProperties (Maybe Int)
tpSizeBytes = lens _tpSizeBytes (\ s a -> s{_tpSizeBytes = a})

-- | The time at which the custom terminology was last update, based on the timestamp.
tpLastUpdatedAt :: Lens' TerminologyProperties (Maybe UTCTime)
tpLastUpdatedAt = lens _tpLastUpdatedAt (\ s a -> s{_tpLastUpdatedAt = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the custom terminology.
tpARN :: Lens' TerminologyProperties (Maybe Text)
tpARN = lens _tpARN (\ s a -> s{_tpARN = a})

-- | The language codes for the target languages available with the custom terminology file. All possible target languages are returned in array.
tpTargetLanguageCodes :: Lens' TerminologyProperties [Text]
tpTargetLanguageCodes = lens _tpTargetLanguageCodes (\ s a -> s{_tpTargetLanguageCodes = a}) . _Default . _Coerce

-- | The time at which the custom terminology was created, based on the timestamp.
tpCreatedAt :: Lens' TerminologyProperties (Maybe UTCTime)
tpCreatedAt = lens _tpCreatedAt (\ s a -> s{_tpCreatedAt = a}) . mapping _Time

-- | The name of the custom terminology.
tpName :: Lens' TerminologyProperties (Maybe Text)
tpName = lens _tpName (\ s a -> s{_tpName = a})

-- | The language code for the source text of the translation request for which the custom terminology is being used.
tpSourceLanguageCode :: Lens' TerminologyProperties (Maybe Text)
tpSourceLanguageCode = lens _tpSourceLanguageCode (\ s a -> s{_tpSourceLanguageCode = a})

-- | The number of terms included in the custom terminology.
tpTermCount :: Lens' TerminologyProperties (Maybe Int)
tpTermCount = lens _tpTermCount (\ s a -> s{_tpTermCount = a})

-- | The encryption key for the custom terminology.
tpEncryptionKey :: Lens' TerminologyProperties (Maybe EncryptionKey)
tpEncryptionKey = lens _tpEncryptionKey (\ s a -> s{_tpEncryptionKey = a})

-- | The description of the custom terminology properties.
tpDescription :: Lens' TerminologyProperties (Maybe Text)
tpDescription = lens _tpDescription (\ s a -> s{_tpDescription = a})

instance FromJSON TerminologyProperties where
        parseJSON
          = withObject "TerminologyProperties"
              (\ x ->
                 TerminologyProperties' <$>
                   (x .:? "SizeBytes") <*> (x .:? "LastUpdatedAt") <*>
                     (x .:? "Arn")
                     <*> (x .:? "TargetLanguageCodes" .!= mempty)
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "Name")
                     <*> (x .:? "SourceLanguageCode")
                     <*> (x .:? "TermCount")
                     <*> (x .:? "EncryptionKey")
                     <*> (x .:? "Description"))

instance Hashable TerminologyProperties where

instance NFData TerminologyProperties where

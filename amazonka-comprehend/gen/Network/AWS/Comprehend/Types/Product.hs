{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.Product where

import Network.AWS.Comprehend.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.
--
--
--
-- /See:/ 'batchDetectDominantLanguageItemResult' smart constructor.
data BatchDetectDominantLanguageItemResult = BatchDetectDominantLanguageItemResult'
  { _bddlirLanguages :: !(Maybe [DominantLanguage])
  , _bddlirIndex     :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDetectDominantLanguageItemResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bddlirLanguages' - One or more 'DominantLanguage' objects describing the dominant languages in the document.
--
-- * 'bddlirIndex' - The zero-based index of the document in the input list.
batchDetectDominantLanguageItemResult
    :: BatchDetectDominantLanguageItemResult
batchDetectDominantLanguageItemResult =
  BatchDetectDominantLanguageItemResult'
    {_bddlirLanguages = Nothing, _bddlirIndex = Nothing}


-- | One or more 'DominantLanguage' objects describing the dominant languages in the document.
bddlirLanguages :: Lens' BatchDetectDominantLanguageItemResult [DominantLanguage]
bddlirLanguages = lens _bddlirLanguages (\ s a -> s{_bddlirLanguages = a}) . _Default . _Coerce

-- | The zero-based index of the document in the input list.
bddlirIndex :: Lens' BatchDetectDominantLanguageItemResult (Maybe Int)
bddlirIndex = lens _bddlirIndex (\ s a -> s{_bddlirIndex = a})

instance FromJSON
           BatchDetectDominantLanguageItemResult
         where
        parseJSON
          = withObject "BatchDetectDominantLanguageItemResult"
              (\ x ->
                 BatchDetectDominantLanguageItemResult' <$>
                   (x .:? "Languages" .!= mempty) <*> (x .:? "Index"))

instance Hashable
           BatchDetectDominantLanguageItemResult
         where

instance NFData BatchDetectDominantLanguageItemResult
         where

-- | The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.
--
--
--
-- /See:/ 'batchDetectEntitiesItemResult' smart constructor.
data BatchDetectEntitiesItemResult = BatchDetectEntitiesItemResult'
  { _bdeirEntities :: !(Maybe [Entity])
  , _bdeirIndex    :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDetectEntitiesItemResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdeirEntities' - One or more 'Entity' objects, one for each entity detected in the document.
--
-- * 'bdeirIndex' - The zero-based index of the document in the input list.
batchDetectEntitiesItemResult
    :: BatchDetectEntitiesItemResult
batchDetectEntitiesItemResult =
  BatchDetectEntitiesItemResult'
    {_bdeirEntities = Nothing, _bdeirIndex = Nothing}


-- | One or more 'Entity' objects, one for each entity detected in the document.
bdeirEntities :: Lens' BatchDetectEntitiesItemResult [Entity]
bdeirEntities = lens _bdeirEntities (\ s a -> s{_bdeirEntities = a}) . _Default . _Coerce

-- | The zero-based index of the document in the input list.
bdeirIndex :: Lens' BatchDetectEntitiesItemResult (Maybe Int)
bdeirIndex = lens _bdeirIndex (\ s a -> s{_bdeirIndex = a})

instance FromJSON BatchDetectEntitiesItemResult where
        parseJSON
          = withObject "BatchDetectEntitiesItemResult"
              (\ x ->
                 BatchDetectEntitiesItemResult' <$>
                   (x .:? "Entities" .!= mempty) <*> (x .:? "Index"))

instance Hashable BatchDetectEntitiesItemResult where

instance NFData BatchDetectEntitiesItemResult where

-- | The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.
--
--
--
-- /See:/ 'batchDetectKeyPhrasesItemResult' smart constructor.
data BatchDetectKeyPhrasesItemResult = BatchDetectKeyPhrasesItemResult'
  { _bdkpirIndex      :: !(Maybe Int)
  , _bdkpirKeyPhrases :: !(Maybe [KeyPhrase])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDetectKeyPhrasesItemResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdkpirIndex' - The zero-based index of the document in the input list.
--
-- * 'bdkpirKeyPhrases' - One or more 'KeyPhrase' objects, one for each key phrase detected in the document.
batchDetectKeyPhrasesItemResult
    :: BatchDetectKeyPhrasesItemResult
batchDetectKeyPhrasesItemResult =
  BatchDetectKeyPhrasesItemResult'
    {_bdkpirIndex = Nothing, _bdkpirKeyPhrases = Nothing}


-- | The zero-based index of the document in the input list.
bdkpirIndex :: Lens' BatchDetectKeyPhrasesItemResult (Maybe Int)
bdkpirIndex = lens _bdkpirIndex (\ s a -> s{_bdkpirIndex = a})

-- | One or more 'KeyPhrase' objects, one for each key phrase detected in the document.
bdkpirKeyPhrases :: Lens' BatchDetectKeyPhrasesItemResult [KeyPhrase]
bdkpirKeyPhrases = lens _bdkpirKeyPhrases (\ s a -> s{_bdkpirKeyPhrases = a}) . _Default . _Coerce

instance FromJSON BatchDetectKeyPhrasesItemResult
         where
        parseJSON
          = withObject "BatchDetectKeyPhrasesItemResult"
              (\ x ->
                 BatchDetectKeyPhrasesItemResult' <$>
                   (x .:? "Index") <*> (x .:? "KeyPhrases" .!= mempty))

instance Hashable BatchDetectKeyPhrasesItemResult
         where

instance NFData BatchDetectKeyPhrasesItemResult where

-- | The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.
--
--
--
-- /See:/ 'batchDetectSentimentItemResult' smart constructor.
data BatchDetectSentimentItemResult = BatchDetectSentimentItemResult'
  { _bdsirSentiment      :: !(Maybe SentimentType)
  , _bdsirSentimentScore :: !(Maybe SentimentScore)
  , _bdsirIndex          :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDetectSentimentItemResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdsirSentiment' - The sentiment detected in the document.
--
-- * 'bdsirSentimentScore' - The level of confidence that Amazon Comprehend has in the accuracy of its sentiment detection.
--
-- * 'bdsirIndex' - The zero-based index of the document in the input list.
batchDetectSentimentItemResult
    :: BatchDetectSentimentItemResult
batchDetectSentimentItemResult =
  BatchDetectSentimentItemResult'
    { _bdsirSentiment = Nothing
    , _bdsirSentimentScore = Nothing
    , _bdsirIndex = Nothing
    }


-- | The sentiment detected in the document.
bdsirSentiment :: Lens' BatchDetectSentimentItemResult (Maybe SentimentType)
bdsirSentiment = lens _bdsirSentiment (\ s a -> s{_bdsirSentiment = a})

-- | The level of confidence that Amazon Comprehend has in the accuracy of its sentiment detection.
bdsirSentimentScore :: Lens' BatchDetectSentimentItemResult (Maybe SentimentScore)
bdsirSentimentScore = lens _bdsirSentimentScore (\ s a -> s{_bdsirSentimentScore = a})

-- | The zero-based index of the document in the input list.
bdsirIndex :: Lens' BatchDetectSentimentItemResult (Maybe Int)
bdsirIndex = lens _bdsirIndex (\ s a -> s{_bdsirIndex = a})

instance FromJSON BatchDetectSentimentItemResult
         where
        parseJSON
          = withObject "BatchDetectSentimentItemResult"
              (\ x ->
                 BatchDetectSentimentItemResult' <$>
                   (x .:? "Sentiment") <*> (x .:? "SentimentScore") <*>
                     (x .:? "Index"))

instance Hashable BatchDetectSentimentItemResult
         where

instance NFData BatchDetectSentimentItemResult where

-- | Describes an error that occurred while processing a document in a batch. The operation returns on @BatchItemError@ object for each document that contained an error.
--
--
--
-- /See:/ 'batchItemError' smart constructor.
data BatchItemError = BatchItemError'
  { _bieErrorCode    :: !(Maybe Text)
  , _bieErrorMessage :: !(Maybe Text)
  , _bieIndex        :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchItemError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bieErrorCode' - The numeric error code of the error.
--
-- * 'bieErrorMessage' - A text description of the error.
--
-- * 'bieIndex' - The zero-based index of the document in the input list.
batchItemError
    :: BatchItemError
batchItemError =
  BatchItemError'
    {_bieErrorCode = Nothing, _bieErrorMessage = Nothing, _bieIndex = Nothing}


-- | The numeric error code of the error.
bieErrorCode :: Lens' BatchItemError (Maybe Text)
bieErrorCode = lens _bieErrorCode (\ s a -> s{_bieErrorCode = a})

-- | A text description of the error.
bieErrorMessage :: Lens' BatchItemError (Maybe Text)
bieErrorMessage = lens _bieErrorMessage (\ s a -> s{_bieErrorMessage = a})

-- | The zero-based index of the document in the input list.
bieIndex :: Lens' BatchItemError (Maybe Int)
bieIndex = lens _bieIndex (\ s a -> s{_bieIndex = a})

instance FromJSON BatchItemError where
        parseJSON
          = withObject "BatchItemError"
              (\ x ->
                 BatchItemError' <$>
                   (x .:? "ErrorCode") <*> (x .:? "ErrorMessage") <*>
                     (x .:? "Index"))

instance Hashable BatchItemError where

instance NFData BatchItemError where

-- | Returns the code for the dominant language in the input text and the level of confidence that Amazon Comprehend has in the accuracy of the detection.
--
--
--
-- /See:/ 'dominantLanguage' smart constructor.
data DominantLanguage = DominantLanguage'
  { _dlLanguageCode :: !(Maybe Text)
  , _dlScore        :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DominantLanguage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlLanguageCode' - The RFC 5646 language code for the dominant language.
--
-- * 'dlScore' - The level of confidence that Amazon Comprehend has in the accuracy of the detection.
dominantLanguage
    :: DominantLanguage
dominantLanguage =
  DominantLanguage' {_dlLanguageCode = Nothing, _dlScore = Nothing}


-- | The RFC 5646 language code for the dominant language.
dlLanguageCode :: Lens' DominantLanguage (Maybe Text)
dlLanguageCode = lens _dlLanguageCode (\ s a -> s{_dlLanguageCode = a})

-- | The level of confidence that Amazon Comprehend has in the accuracy of the detection.
dlScore :: Lens' DominantLanguage (Maybe Double)
dlScore = lens _dlScore (\ s a -> s{_dlScore = a})

instance FromJSON DominantLanguage where
        parseJSON
          = withObject "DominantLanguage"
              (\ x ->
                 DominantLanguage' <$>
                   (x .:? "LanguageCode") <*> (x .:? "Score"))

instance Hashable DominantLanguage where

instance NFData DominantLanguage where

-- | Provides information about an entity.
--
--
--
--
--
-- /See:/ 'entity' smart constructor.
data Entity = Entity'
  { _eBeginOffset :: !(Maybe Int)
  , _eText        :: !(Maybe Text)
  , _eScore       :: !(Maybe Double)
  , _eEndOffset   :: !(Maybe Int)
  , _eType        :: !(Maybe EntityType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Entity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eBeginOffset' - A character offset in the input text that shows where the entity begins (the first character is at position 0). The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
--
-- * 'eText' - The text of the entity.
--
-- * 'eScore' - The level of confidence that Amazon Comprehend has in the accuracy of the detection.
--
-- * 'eEndOffset' - A character offset in the input text that shows where the entity ends. The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
--
-- * 'eType' - The entity's type.
entity
    :: Entity
entity =
  Entity'
    { _eBeginOffset = Nothing
    , _eText = Nothing
    , _eScore = Nothing
    , _eEndOffset = Nothing
    , _eType = Nothing
    }


-- | A character offset in the input text that shows where the entity begins (the first character is at position 0). The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
eBeginOffset :: Lens' Entity (Maybe Int)
eBeginOffset = lens _eBeginOffset (\ s a -> s{_eBeginOffset = a})

-- | The text of the entity.
eText :: Lens' Entity (Maybe Text)
eText = lens _eText (\ s a -> s{_eText = a})

-- | The level of confidence that Amazon Comprehend has in the accuracy of the detection.
eScore :: Lens' Entity (Maybe Double)
eScore = lens _eScore (\ s a -> s{_eScore = a})

-- | A character offset in the input text that shows where the entity ends. The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
eEndOffset :: Lens' Entity (Maybe Int)
eEndOffset = lens _eEndOffset (\ s a -> s{_eEndOffset = a})

-- | The entity's type.
eType :: Lens' Entity (Maybe EntityType)
eType = lens _eType (\ s a -> s{_eType = a})

instance FromJSON Entity where
        parseJSON
          = withObject "Entity"
              (\ x ->
                 Entity' <$>
                   (x .:? "BeginOffset") <*> (x .:? "Text") <*>
                     (x .:? "Score")
                     <*> (x .:? "EndOffset")
                     <*> (x .:? "Type"))

instance Hashable Entity where

instance NFData Entity where

-- | The input properties for a topic detection job.
--
--
--
-- /See:/ 'inputDataConfig' smart constructor.
data InputDataConfig = InputDataConfig'
  { _idcInputFormat :: !(Maybe InputFormat)
  , _idcS3URI       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputDataConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idcInputFormat' - Specifies how the text in an input file should be processed:     * @ONE_DOC_PER_FILE@ - Each file is considered a separate document. Use this option when you are processing large documents, such as newspaper articles or scientific papers.     * @ONE_DOC_PER_LINE@ - Each line in a file is considered a separate document. Use this option when you are processing many short documents, such as text messages.
--
-- * 'idcS3URI' - The Amazon S3 URI for the input data. The URI must be in same region as the API endpoint that you are calling. The URI can point to a single input file or it can provide the prefix for a collection of data files.  For example, if you use the URI @S3://bucketName/prefix@ , if the prefix is a single file, Amazon Comprehend uses that file as input. If more than one file begins with the prefix, Amazon Comprehend uses all of them as input.
inputDataConfig
    :: Text -- ^ 'idcS3URI'
    -> InputDataConfig
inputDataConfig pS3URI_ =
  InputDataConfig' {_idcInputFormat = Nothing, _idcS3URI = pS3URI_}


-- | Specifies how the text in an input file should be processed:     * @ONE_DOC_PER_FILE@ - Each file is considered a separate document. Use this option when you are processing large documents, such as newspaper articles or scientific papers.     * @ONE_DOC_PER_LINE@ - Each line in a file is considered a separate document. Use this option when you are processing many short documents, such as text messages.
idcInputFormat :: Lens' InputDataConfig (Maybe InputFormat)
idcInputFormat = lens _idcInputFormat (\ s a -> s{_idcInputFormat = a})

-- | The Amazon S3 URI for the input data. The URI must be in same region as the API endpoint that you are calling. The URI can point to a single input file or it can provide the prefix for a collection of data files.  For example, if you use the URI @S3://bucketName/prefix@ , if the prefix is a single file, Amazon Comprehend uses that file as input. If more than one file begins with the prefix, Amazon Comprehend uses all of them as input.
idcS3URI :: Lens' InputDataConfig Text
idcS3URI = lens _idcS3URI (\ s a -> s{_idcS3URI = a})

instance FromJSON InputDataConfig where
        parseJSON
          = withObject "InputDataConfig"
              (\ x ->
                 InputDataConfig' <$>
                   (x .:? "InputFormat") <*> (x .: "S3Uri"))

instance Hashable InputDataConfig where

instance NFData InputDataConfig where

instance ToJSON InputDataConfig where
        toJSON InputDataConfig'{..}
          = object
              (catMaybes
                 [("InputFormat" .=) <$> _idcInputFormat,
                  Just ("S3Uri" .= _idcS3URI)])

-- | Describes a key noun phrase.
--
--
--
-- /See:/ 'keyPhrase' smart constructor.
data KeyPhrase = KeyPhrase'
  { _kpBeginOffset :: !(Maybe Int)
  , _kpText        :: !(Maybe Text)
  , _kpScore       :: !(Maybe Double)
  , _kpEndOffset   :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KeyPhrase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kpBeginOffset' - A character offset in the input text that shows where the key phrase begins (the first character is at position 0). The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
--
-- * 'kpText' - The text of a key noun phrase.
--
-- * 'kpScore' - The level of confidence that Amazon Comprehend has in the accuracy of the detection.
--
-- * 'kpEndOffset' - A character offset in the input text where the key phrase ends. The offset returns the position of each UTF-8 code point in the string. A @code point@ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
keyPhrase
    :: KeyPhrase
keyPhrase =
  KeyPhrase'
    { _kpBeginOffset = Nothing
    , _kpText = Nothing
    , _kpScore = Nothing
    , _kpEndOffset = Nothing
    }


-- | A character offset in the input text that shows where the key phrase begins (the first character is at position 0). The offset returns the position of each UTF-8 code point in the string. A /code point/ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
kpBeginOffset :: Lens' KeyPhrase (Maybe Int)
kpBeginOffset = lens _kpBeginOffset (\ s a -> s{_kpBeginOffset = a})

-- | The text of a key noun phrase.
kpText :: Lens' KeyPhrase (Maybe Text)
kpText = lens _kpText (\ s a -> s{_kpText = a})

-- | The level of confidence that Amazon Comprehend has in the accuracy of the detection.
kpScore :: Lens' KeyPhrase (Maybe Double)
kpScore = lens _kpScore (\ s a -> s{_kpScore = a})

-- | A character offset in the input text where the key phrase ends. The offset returns the position of each UTF-8 code point in the string. A @code point@ is the abstract character from a particular graphical representation. For example, a multi-byte UTF-8 character maps to a single code point.
kpEndOffset :: Lens' KeyPhrase (Maybe Int)
kpEndOffset = lens _kpEndOffset (\ s a -> s{_kpEndOffset = a})

instance FromJSON KeyPhrase where
        parseJSON
          = withObject "KeyPhrase"
              (\ x ->
                 KeyPhrase' <$>
                   (x .:? "BeginOffset") <*> (x .:? "Text") <*>
                     (x .:? "Score")
                     <*> (x .:? "EndOffset"))

instance Hashable KeyPhrase where

instance NFData KeyPhrase where

-- | Provides configuration parameters for the output of topic detection jobs.
--
--
--
--
--
-- /See:/ 'outputDataConfig' smart constructor.
newtype OutputDataConfig = OutputDataConfig'
  { _odcS3URI :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OutputDataConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'odcS3URI' - The Amazon S3 URI where you want to write the output data. The URI must be in the same region as the API endpoint that you are calling.  The service creates an output file called @output.tar.gz@ . It is a compressed archive that contains two files, @topic-terms.csv@ that lists the terms associated with each topic, and @doc-topics.csv@ that lists the documents associated with each topic. For more information, see 'topic-modeling' .
outputDataConfig
    :: Text -- ^ 'odcS3URI'
    -> OutputDataConfig
outputDataConfig pS3URI_ = OutputDataConfig' {_odcS3URI = pS3URI_}


-- | The Amazon S3 URI where you want to write the output data. The URI must be in the same region as the API endpoint that you are calling.  The service creates an output file called @output.tar.gz@ . It is a compressed archive that contains two files, @topic-terms.csv@ that lists the terms associated with each topic, and @doc-topics.csv@ that lists the documents associated with each topic. For more information, see 'topic-modeling' .
odcS3URI :: Lens' OutputDataConfig Text
odcS3URI = lens _odcS3URI (\ s a -> s{_odcS3URI = a})

instance FromJSON OutputDataConfig where
        parseJSON
          = withObject "OutputDataConfig"
              (\ x -> OutputDataConfig' <$> (x .: "S3Uri"))

instance Hashable OutputDataConfig where

instance NFData OutputDataConfig where

instance ToJSON OutputDataConfig where
        toJSON OutputDataConfig'{..}
          = object (catMaybes [Just ("S3Uri" .= _odcS3URI)])

-- | Describes the level of confidence that Amazon Comprehend has in the accuracy of its detection of sentiments.
--
--
--
-- /See:/ 'sentimentScore' smart constructor.
data SentimentScore = SentimentScore'
  { _ssMixed    :: !(Maybe Double)
  , _ssNegative :: !(Maybe Double)
  , _ssNeutral  :: !(Maybe Double)
  , _ssPositive :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SentimentScore' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssMixed' - The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @MIXED@ sentiment.
--
-- * 'ssNegative' - The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @NEGATIVE@ sentiment.
--
-- * 'ssNeutral' - The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @NEUTRAL@ sentiment.
--
-- * 'ssPositive' - The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @POSITIVE@ sentiment.
sentimentScore
    :: SentimentScore
sentimentScore =
  SentimentScore'
    { _ssMixed = Nothing
    , _ssNegative = Nothing
    , _ssNeutral = Nothing
    , _ssPositive = Nothing
    }


-- | The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @MIXED@ sentiment.
ssMixed :: Lens' SentimentScore (Maybe Double)
ssMixed = lens _ssMixed (\ s a -> s{_ssMixed = a})

-- | The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @NEGATIVE@ sentiment.
ssNegative :: Lens' SentimentScore (Maybe Double)
ssNegative = lens _ssNegative (\ s a -> s{_ssNegative = a})

-- | The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @NEUTRAL@ sentiment.
ssNeutral :: Lens' SentimentScore (Maybe Double)
ssNeutral = lens _ssNeutral (\ s a -> s{_ssNeutral = a})

-- | The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @POSITIVE@ sentiment.
ssPositive :: Lens' SentimentScore (Maybe Double)
ssPositive = lens _ssPositive (\ s a -> s{_ssPositive = a})

instance FromJSON SentimentScore where
        parseJSON
          = withObject "SentimentScore"
              (\ x ->
                 SentimentScore' <$>
                   (x .:? "Mixed") <*> (x .:? "Negative") <*>
                     (x .:? "Neutral")
                     <*> (x .:? "Positive"))

instance Hashable SentimentScore where

instance NFData SentimentScore where

-- | Provides information for filtering topic detection jobs. For more information, see .
--
--
--
-- /See:/ 'topicsDetectionJobFilter' smart constructor.
data TopicsDetectionJobFilter = TopicsDetectionJobFilter'
  { _tdjfSubmitTimeAfter  :: !(Maybe POSIX)
  , _tdjfSubmitTimeBefore :: !(Maybe POSIX)
  , _tdjfJobName          :: !(Maybe Text)
  , _tdjfJobStatus        :: !(Maybe JobStatus)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TopicsDetectionJobFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdjfSubmitTimeAfter' - Filters the list of jobs based on the time that the job was submitted for processing. Only returns jobs submitted after the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- * 'tdjfSubmitTimeBefore' - Filters the list of jobs based on the time that the job was submitted for processing. Only returns jobs submitted before the specified time. Jobs are returned in descending order, newest to oldest.
--
-- * 'tdjfJobName' -
--
-- * 'tdjfJobStatus' - Filters the list of topic detection jobs based on job status. Returns only jobs with the specified status.
topicsDetectionJobFilter
    :: TopicsDetectionJobFilter
topicsDetectionJobFilter =
  TopicsDetectionJobFilter'
    { _tdjfSubmitTimeAfter = Nothing
    , _tdjfSubmitTimeBefore = Nothing
    , _tdjfJobName = Nothing
    , _tdjfJobStatus = Nothing
    }


-- | Filters the list of jobs based on the time that the job was submitted for processing. Only returns jobs submitted after the specified time. Jobs are returned in ascending order, oldest to newest.
tdjfSubmitTimeAfter :: Lens' TopicsDetectionJobFilter (Maybe UTCTime)
tdjfSubmitTimeAfter = lens _tdjfSubmitTimeAfter (\ s a -> s{_tdjfSubmitTimeAfter = a}) . mapping _Time

-- | Filters the list of jobs based on the time that the job was submitted for processing. Only returns jobs submitted before the specified time. Jobs are returned in descending order, newest to oldest.
tdjfSubmitTimeBefore :: Lens' TopicsDetectionJobFilter (Maybe UTCTime)
tdjfSubmitTimeBefore = lens _tdjfSubmitTimeBefore (\ s a -> s{_tdjfSubmitTimeBefore = a}) . mapping _Time

-- |
tdjfJobName :: Lens' TopicsDetectionJobFilter (Maybe Text)
tdjfJobName = lens _tdjfJobName (\ s a -> s{_tdjfJobName = a})

-- | Filters the list of topic detection jobs based on job status. Returns only jobs with the specified status.
tdjfJobStatus :: Lens' TopicsDetectionJobFilter (Maybe JobStatus)
tdjfJobStatus = lens _tdjfJobStatus (\ s a -> s{_tdjfJobStatus = a})

instance Hashable TopicsDetectionJobFilter where

instance NFData TopicsDetectionJobFilter where

instance ToJSON TopicsDetectionJobFilter where
        toJSON TopicsDetectionJobFilter'{..}
          = object
              (catMaybes
                 [("SubmitTimeAfter" .=) <$> _tdjfSubmitTimeAfter,
                  ("SubmitTimeBefore" .=) <$> _tdjfSubmitTimeBefore,
                  ("JobName" .=) <$> _tdjfJobName,
                  ("JobStatus" .=) <$> _tdjfJobStatus])

-- | Provides information about a topic detection job.
--
--
--
-- /See:/ 'topicsDetectionJobProperties' smart constructor.
data TopicsDetectionJobProperties = TopicsDetectionJobProperties'
  { _tdjpJobId            :: !(Maybe Text)
  , _tdjpJobName          :: !(Maybe Text)
  , _tdjpInputDataConfig  :: !(Maybe InputDataConfig)
  , _tdjpEndTime          :: !(Maybe POSIX)
  , _tdjpOutputDataConfig :: !(Maybe OutputDataConfig)
  , _tdjpNumberOfTopics   :: !(Maybe Int)
  , _tdjpJobStatus        :: !(Maybe JobStatus)
  , _tdjpMessage          :: !(Maybe Text)
  , _tdjpSubmitTime       :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TopicsDetectionJobProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdjpJobId' - The identifier assigned to the topic detection job.
--
-- * 'tdjpJobName' - The name of the topic detection job.
--
-- * 'tdjpInputDataConfig' - The input data configuration supplied when you created the topic detection job.
--
-- * 'tdjpEndTime' - The time that the topic detection job was completed.
--
-- * 'tdjpOutputDataConfig' - The output data configuration supplied when you created the topic detection job.
--
-- * 'tdjpNumberOfTopics' - The number of topics to detect supplied when you created the topic detection job. The default is 10.
--
-- * 'tdjpJobStatus' - The current status of the topic detection job. If the status is @Failed@ , the reason for the failure is shown in the @Message@ field.
--
-- * 'tdjpMessage' - A description for the status of a job.
--
-- * 'tdjpSubmitTime' - The time that the topic detection job was submitted for processing.
topicsDetectionJobProperties
    :: TopicsDetectionJobProperties
topicsDetectionJobProperties =
  TopicsDetectionJobProperties'
    { _tdjpJobId = Nothing
    , _tdjpJobName = Nothing
    , _tdjpInputDataConfig = Nothing
    , _tdjpEndTime = Nothing
    , _tdjpOutputDataConfig = Nothing
    , _tdjpNumberOfTopics = Nothing
    , _tdjpJobStatus = Nothing
    , _tdjpMessage = Nothing
    , _tdjpSubmitTime = Nothing
    }


-- | The identifier assigned to the topic detection job.
tdjpJobId :: Lens' TopicsDetectionJobProperties (Maybe Text)
tdjpJobId = lens _tdjpJobId (\ s a -> s{_tdjpJobId = a})

-- | The name of the topic detection job.
tdjpJobName :: Lens' TopicsDetectionJobProperties (Maybe Text)
tdjpJobName = lens _tdjpJobName (\ s a -> s{_tdjpJobName = a})

-- | The input data configuration supplied when you created the topic detection job.
tdjpInputDataConfig :: Lens' TopicsDetectionJobProperties (Maybe InputDataConfig)
tdjpInputDataConfig = lens _tdjpInputDataConfig (\ s a -> s{_tdjpInputDataConfig = a})

-- | The time that the topic detection job was completed.
tdjpEndTime :: Lens' TopicsDetectionJobProperties (Maybe UTCTime)
tdjpEndTime = lens _tdjpEndTime (\ s a -> s{_tdjpEndTime = a}) . mapping _Time

-- | The output data configuration supplied when you created the topic detection job.
tdjpOutputDataConfig :: Lens' TopicsDetectionJobProperties (Maybe OutputDataConfig)
tdjpOutputDataConfig = lens _tdjpOutputDataConfig (\ s a -> s{_tdjpOutputDataConfig = a})

-- | The number of topics to detect supplied when you created the topic detection job. The default is 10.
tdjpNumberOfTopics :: Lens' TopicsDetectionJobProperties (Maybe Int)
tdjpNumberOfTopics = lens _tdjpNumberOfTopics (\ s a -> s{_tdjpNumberOfTopics = a})

-- | The current status of the topic detection job. If the status is @Failed@ , the reason for the failure is shown in the @Message@ field.
tdjpJobStatus :: Lens' TopicsDetectionJobProperties (Maybe JobStatus)
tdjpJobStatus = lens _tdjpJobStatus (\ s a -> s{_tdjpJobStatus = a})

-- | A description for the status of a job.
tdjpMessage :: Lens' TopicsDetectionJobProperties (Maybe Text)
tdjpMessage = lens _tdjpMessage (\ s a -> s{_tdjpMessage = a})

-- | The time that the topic detection job was submitted for processing.
tdjpSubmitTime :: Lens' TopicsDetectionJobProperties (Maybe UTCTime)
tdjpSubmitTime = lens _tdjpSubmitTime (\ s a -> s{_tdjpSubmitTime = a}) . mapping _Time

instance FromJSON TopicsDetectionJobProperties where
        parseJSON
          = withObject "TopicsDetectionJobProperties"
              (\ x ->
                 TopicsDetectionJobProperties' <$>
                   (x .:? "JobId") <*> (x .:? "JobName") <*>
                     (x .:? "InputDataConfig")
                     <*> (x .:? "EndTime")
                     <*> (x .:? "OutputDataConfig")
                     <*> (x .:? "NumberOfTopics")
                     <*> (x .:? "JobStatus")
                     <*> (x .:? "Message")
                     <*> (x .:? "SubmitTime"))

instance Hashable TopicsDetectionJobProperties where

instance NFData TopicsDetectionJobProperties where

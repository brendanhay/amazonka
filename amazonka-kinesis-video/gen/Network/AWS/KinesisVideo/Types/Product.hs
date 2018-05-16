{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideo.Types.Product where

import Network.AWS.KinesisVideo.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object describing a Kinesis video stream.
--
--
--
-- /See:/ 'streamInfo' smart constructor.
data StreamInfo = StreamInfo'
  { _siCreationTime         :: !(Maybe POSIX)
  , _siStatus               :: !(Maybe StreamStatus)
  , _siMediaType            :: !(Maybe Text)
  , _siDataRetentionInHours :: !(Maybe Nat)
  , _siStreamARN            :: !(Maybe Text)
  , _siKMSKeyId             :: !(Maybe Text)
  , _siDeviceName           :: !(Maybe Text)
  , _siVersion              :: !(Maybe Text)
  , _siStreamName           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StreamInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siCreationTime' - A time stamp that indicates when the stream was created.
--
-- * 'siStatus' - The status of the stream.
--
-- * 'siMediaType' - The @MediaType@ of the stream.
--
-- * 'siDataRetentionInHours' - How long the stream retains data, in hours.
--
-- * 'siStreamARN' - The Amazon Resource Name (ARN) of the stream.
--
-- * 'siKMSKeyId' - The ID of the AWS Key Management Service (AWS KMS) key that Kinesis Video Streams uses to encrypt data on the stream.
--
-- * 'siDeviceName' - The name of the device that is associated with the stream.
--
-- * 'siVersion' - The version of the stream.
--
-- * 'siStreamName' - The name of the stream.
streamInfo
    :: StreamInfo
streamInfo =
  StreamInfo'
    { _siCreationTime = Nothing
    , _siStatus = Nothing
    , _siMediaType = Nothing
    , _siDataRetentionInHours = Nothing
    , _siStreamARN = Nothing
    , _siKMSKeyId = Nothing
    , _siDeviceName = Nothing
    , _siVersion = Nothing
    , _siStreamName = Nothing
    }


-- | A time stamp that indicates when the stream was created.
siCreationTime :: Lens' StreamInfo (Maybe UTCTime)
siCreationTime = lens _siCreationTime (\ s a -> s{_siCreationTime = a}) . mapping _Time

-- | The status of the stream.
siStatus :: Lens' StreamInfo (Maybe StreamStatus)
siStatus = lens _siStatus (\ s a -> s{_siStatus = a})

-- | The @MediaType@ of the stream.
siMediaType :: Lens' StreamInfo (Maybe Text)
siMediaType = lens _siMediaType (\ s a -> s{_siMediaType = a})

-- | How long the stream retains data, in hours.
siDataRetentionInHours :: Lens' StreamInfo (Maybe Natural)
siDataRetentionInHours = lens _siDataRetentionInHours (\ s a -> s{_siDataRetentionInHours = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the stream.
siStreamARN :: Lens' StreamInfo (Maybe Text)
siStreamARN = lens _siStreamARN (\ s a -> s{_siStreamARN = a})

-- | The ID of the AWS Key Management Service (AWS KMS) key that Kinesis Video Streams uses to encrypt data on the stream.
siKMSKeyId :: Lens' StreamInfo (Maybe Text)
siKMSKeyId = lens _siKMSKeyId (\ s a -> s{_siKMSKeyId = a})

-- | The name of the device that is associated with the stream.
siDeviceName :: Lens' StreamInfo (Maybe Text)
siDeviceName = lens _siDeviceName (\ s a -> s{_siDeviceName = a})

-- | The version of the stream.
siVersion :: Lens' StreamInfo (Maybe Text)
siVersion = lens _siVersion (\ s a -> s{_siVersion = a})

-- | The name of the stream.
siStreamName :: Lens' StreamInfo (Maybe Text)
siStreamName = lens _siStreamName (\ s a -> s{_siStreamName = a})

instance FromJSON StreamInfo where
        parseJSON
          = withObject "StreamInfo"
              (\ x ->
                 StreamInfo' <$>
                   (x .:? "CreationTime") <*> (x .:? "Status") <*>
                     (x .:? "MediaType")
                     <*> (x .:? "DataRetentionInHours")
                     <*> (x .:? "StreamARN")
                     <*> (x .:? "KmsKeyId")
                     <*> (x .:? "DeviceName")
                     <*> (x .:? "Version")
                     <*> (x .:? "StreamName"))

instance Hashable StreamInfo where

instance NFData StreamInfo where

-- | Specifies the condition that streams must satisfy to be returned when you list streams (see the @ListStreams@ API). A condition has a comparison operation and a value. Currently, you can specify only the @BEGINS_WITH@ operator, which finds streams whose names start with a given prefix.
--
--
--
-- /See:/ 'streamNameCondition' smart constructor.
data StreamNameCondition = StreamNameCondition'
  { _sncComparisonOperator :: !(Maybe ComparisonOperator)
  , _sncComparisonValue    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StreamNameCondition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sncComparisonOperator' - A comparison operator. Currently, you can specify only the @BEGINS_WITH@ operator, which finds streams whose names start with a given prefix.
--
-- * 'sncComparisonValue' - A value to compare.
streamNameCondition
    :: StreamNameCondition
streamNameCondition =
  StreamNameCondition'
    {_sncComparisonOperator = Nothing, _sncComparisonValue = Nothing}


-- | A comparison operator. Currently, you can specify only the @BEGINS_WITH@ operator, which finds streams whose names start with a given prefix.
sncComparisonOperator :: Lens' StreamNameCondition (Maybe ComparisonOperator)
sncComparisonOperator = lens _sncComparisonOperator (\ s a -> s{_sncComparisonOperator = a})

-- | A value to compare.
sncComparisonValue :: Lens' StreamNameCondition (Maybe Text)
sncComparisonValue = lens _sncComparisonValue (\ s a -> s{_sncComparisonValue = a})

instance Hashable StreamNameCondition where

instance NFData StreamNameCondition where

instance ToJSON StreamNameCondition where
        toJSON StreamNameCondition'{..}
          = object
              (catMaybes
                 [("ComparisonOperator" .=) <$>
                    _sncComparisonOperator,
                  ("ComparisonValue" .=) <$> _sncComparisonValue])

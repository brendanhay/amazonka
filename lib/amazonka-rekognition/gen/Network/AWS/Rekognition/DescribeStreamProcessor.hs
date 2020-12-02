{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DescribeStreamProcessor
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about a stream processor created by . You can get information about the input and output streams, the input parameters for the face recognition being performed, and the current status of the stream processor.
--
--
module Network.AWS.Rekognition.DescribeStreamProcessor
    (
    -- * Creating a Request
      describeStreamProcessor
    , DescribeStreamProcessor
    -- * Request Lenses
    , dspName

    -- * Destructuring the Response
    , describeStreamProcessorResponse
    , DescribeStreamProcessorResponse
    -- * Response Lenses
    , dsprsStatus
    , dsprsSettings
    , dsprsInput
    , dsprsOutput
    , dsprsStreamProcessorARN
    , dsprsStatusMessage
    , dsprsName
    , dsprsCreationTimestamp
    , dsprsLastUpdateTimestamp
    , dsprsRoleARN
    , dsprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeStreamProcessor' smart constructor.
newtype DescribeStreamProcessor = DescribeStreamProcessor'
  { _dspName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStreamProcessor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dspName' - Name of the stream processor for which you want information.
describeStreamProcessor
    :: Text -- ^ 'dspName'
    -> DescribeStreamProcessor
describeStreamProcessor pName_ = DescribeStreamProcessor' {_dspName = pName_}


-- | Name of the stream processor for which you want information.
dspName :: Lens' DescribeStreamProcessor Text
dspName = lens _dspName (\ s a -> s{_dspName = a})

instance AWSRequest DescribeStreamProcessor where
        type Rs DescribeStreamProcessor =
             DescribeStreamProcessorResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 DescribeStreamProcessorResponse' <$>
                   (x .?> "Status") <*> (x .?> "Settings") <*>
                     (x .?> "Input")
                     <*> (x .?> "Output")
                     <*> (x .?> "StreamProcessorArn")
                     <*> (x .?> "StatusMessage")
                     <*> (x .?> "Name")
                     <*> (x .?> "CreationTimestamp")
                     <*> (x .?> "LastUpdateTimestamp")
                     <*> (x .?> "RoleArn")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeStreamProcessor where

instance NFData DescribeStreamProcessor where

instance ToHeaders DescribeStreamProcessor where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.DescribeStreamProcessor" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeStreamProcessor where
        toJSON DescribeStreamProcessor'{..}
          = object (catMaybes [Just ("Name" .= _dspName)])

instance ToPath DescribeStreamProcessor where
        toPath = const "/"

instance ToQuery DescribeStreamProcessor where
        toQuery = const mempty

-- | /See:/ 'describeStreamProcessorResponse' smart constructor.
data DescribeStreamProcessorResponse = DescribeStreamProcessorResponse'
  { _dsprsStatus              :: !(Maybe StreamProcessorStatus)
  , _dsprsSettings            :: !(Maybe StreamProcessorSettings)
  , _dsprsInput               :: !(Maybe StreamProcessorInput)
  , _dsprsOutput              :: !(Maybe StreamProcessorOutput)
  , _dsprsStreamProcessorARN  :: !(Maybe Text)
  , _dsprsStatusMessage       :: !(Maybe Text)
  , _dsprsName                :: !(Maybe Text)
  , _dsprsCreationTimestamp   :: !(Maybe POSIX)
  , _dsprsLastUpdateTimestamp :: !(Maybe POSIX)
  , _dsprsRoleARN             :: !(Maybe Text)
  , _dsprsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStreamProcessorResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsprsStatus' - Current status of the stream processor.
--
-- * 'dsprsSettings' - Face recognition input parameters that are being used by the stream processor. Includes the collection to use for face recognition and the face attributes to detect.
--
-- * 'dsprsInput' - Kinesis video stream that provides the source streaming video.
--
-- * 'dsprsOutput' - Kinesis data stream to which Rekognition Video puts the analysis results.
--
-- * 'dsprsStreamProcessorARN' - ARN of the stream processor.
--
-- * 'dsprsStatusMessage' - Detailed status message about the stream processor.
--
-- * 'dsprsName' - Name of the stream processor.
--
-- * 'dsprsCreationTimestamp' - Date and time the stream processor was created
--
-- * 'dsprsLastUpdateTimestamp' - The time, in Unix format, the stream processor was last updated. For example, when the stream processor moves from a running state to a failed state, or when the user starts or stops the stream processor.
--
-- * 'dsprsRoleARN' - ARN of the IAM role that allows access to the stream processor.
--
-- * 'dsprsResponseStatus' - -- | The response status code.
describeStreamProcessorResponse
    :: Int -- ^ 'dsprsResponseStatus'
    -> DescribeStreamProcessorResponse
describeStreamProcessorResponse pResponseStatus_ =
  DescribeStreamProcessorResponse'
    { _dsprsStatus = Nothing
    , _dsprsSettings = Nothing
    , _dsprsInput = Nothing
    , _dsprsOutput = Nothing
    , _dsprsStreamProcessorARN = Nothing
    , _dsprsStatusMessage = Nothing
    , _dsprsName = Nothing
    , _dsprsCreationTimestamp = Nothing
    , _dsprsLastUpdateTimestamp = Nothing
    , _dsprsRoleARN = Nothing
    , _dsprsResponseStatus = pResponseStatus_
    }


-- | Current status of the stream processor.
dsprsStatus :: Lens' DescribeStreamProcessorResponse (Maybe StreamProcessorStatus)
dsprsStatus = lens _dsprsStatus (\ s a -> s{_dsprsStatus = a})

-- | Face recognition input parameters that are being used by the stream processor. Includes the collection to use for face recognition and the face attributes to detect.
dsprsSettings :: Lens' DescribeStreamProcessorResponse (Maybe StreamProcessorSettings)
dsprsSettings = lens _dsprsSettings (\ s a -> s{_dsprsSettings = a})

-- | Kinesis video stream that provides the source streaming video.
dsprsInput :: Lens' DescribeStreamProcessorResponse (Maybe StreamProcessorInput)
dsprsInput = lens _dsprsInput (\ s a -> s{_dsprsInput = a})

-- | Kinesis data stream to which Rekognition Video puts the analysis results.
dsprsOutput :: Lens' DescribeStreamProcessorResponse (Maybe StreamProcessorOutput)
dsprsOutput = lens _dsprsOutput (\ s a -> s{_dsprsOutput = a})

-- | ARN of the stream processor.
dsprsStreamProcessorARN :: Lens' DescribeStreamProcessorResponse (Maybe Text)
dsprsStreamProcessorARN = lens _dsprsStreamProcessorARN (\ s a -> s{_dsprsStreamProcessorARN = a})

-- | Detailed status message about the stream processor.
dsprsStatusMessage :: Lens' DescribeStreamProcessorResponse (Maybe Text)
dsprsStatusMessage = lens _dsprsStatusMessage (\ s a -> s{_dsprsStatusMessage = a})

-- | Name of the stream processor.
dsprsName :: Lens' DescribeStreamProcessorResponse (Maybe Text)
dsprsName = lens _dsprsName (\ s a -> s{_dsprsName = a})

-- | Date and time the stream processor was created
dsprsCreationTimestamp :: Lens' DescribeStreamProcessorResponse (Maybe UTCTime)
dsprsCreationTimestamp = lens _dsprsCreationTimestamp (\ s a -> s{_dsprsCreationTimestamp = a}) . mapping _Time

-- | The time, in Unix format, the stream processor was last updated. For example, when the stream processor moves from a running state to a failed state, or when the user starts or stops the stream processor.
dsprsLastUpdateTimestamp :: Lens' DescribeStreamProcessorResponse (Maybe UTCTime)
dsprsLastUpdateTimestamp = lens _dsprsLastUpdateTimestamp (\ s a -> s{_dsprsLastUpdateTimestamp = a}) . mapping _Time

-- | ARN of the IAM role that allows access to the stream processor.
dsprsRoleARN :: Lens' DescribeStreamProcessorResponse (Maybe Text)
dsprsRoleARN = lens _dsprsRoleARN (\ s a -> s{_dsprsRoleARN = a})

-- | -- | The response status code.
dsprsResponseStatus :: Lens' DescribeStreamProcessorResponse Int
dsprsResponseStatus = lens _dsprsResponseStatus (\ s a -> s{_dsprsResponseStatus = a})

instance NFData DescribeStreamProcessorResponse where

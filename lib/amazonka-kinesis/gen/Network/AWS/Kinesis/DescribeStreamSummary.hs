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
-- Module      : Network.AWS.Kinesis.DescribeStreamSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a summarized description of the specified Kinesis data stream without the shard list.
--
--
-- The information returned includes the stream name, Amazon Resource Name (ARN), status, record retention period, approximate creation time, monitoring, encryption details, and open shard count.
--
module Network.AWS.Kinesis.DescribeStreamSummary
    (
    -- * Creating a Request
      describeStreamSummary
    , DescribeStreamSummary
    -- * Request Lenses
    , dssStreamName

    -- * Destructuring the Response
    , describeStreamSummaryResponse
    , DescribeStreamSummaryResponse
    -- * Response Lenses
    , dssrsResponseStatus
    , dssrsStreamDescriptionSummary
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Kinesis.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeStreamSummary' smart constructor.
newtype DescribeStreamSummary = DescribeStreamSummary'
  { _dssStreamName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStreamSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssStreamName' - The name of the stream to describe.
describeStreamSummary
    :: Text -- ^ 'dssStreamName'
    -> DescribeStreamSummary
describeStreamSummary pStreamName_ =
  DescribeStreamSummary' {_dssStreamName = pStreamName_}


-- | The name of the stream to describe.
dssStreamName :: Lens' DescribeStreamSummary Text
dssStreamName = lens _dssStreamName (\ s a -> s{_dssStreamName = a})

instance AWSRequest DescribeStreamSummary where
        type Rs DescribeStreamSummary =
             DescribeStreamSummaryResponse
        request = postJSON kinesis
        response
          = receiveJSON
              (\ s h x ->
                 DescribeStreamSummaryResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .:> "StreamDescriptionSummary"))

instance Hashable DescribeStreamSummary where

instance NFData DescribeStreamSummary where

instance ToHeaders DescribeStreamSummary where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.DescribeStreamSummary" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeStreamSummary where
        toJSON DescribeStreamSummary'{..}
          = object
              (catMaybes [Just ("StreamName" .= _dssStreamName)])

instance ToPath DescribeStreamSummary where
        toPath = const "/"

instance ToQuery DescribeStreamSummary where
        toQuery = const mempty

-- | /See:/ 'describeStreamSummaryResponse' smart constructor.
data DescribeStreamSummaryResponse = DescribeStreamSummaryResponse'
  { _dssrsResponseStatus           :: !Int
  , _dssrsStreamDescriptionSummary :: !StreamDescriptionSummary
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStreamSummaryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssrsResponseStatus' - -- | The response status code.
--
-- * 'dssrsStreamDescriptionSummary' - A 'StreamDescriptionSummary' containing information about the stream.
describeStreamSummaryResponse
    :: Int -- ^ 'dssrsResponseStatus'
    -> StreamDescriptionSummary -- ^ 'dssrsStreamDescriptionSummary'
    -> DescribeStreamSummaryResponse
describeStreamSummaryResponse pResponseStatus_ pStreamDescriptionSummary_ =
  DescribeStreamSummaryResponse'
    { _dssrsResponseStatus = pResponseStatus_
    , _dssrsStreamDescriptionSummary = pStreamDescriptionSummary_
    }


-- | -- | The response status code.
dssrsResponseStatus :: Lens' DescribeStreamSummaryResponse Int
dssrsResponseStatus = lens _dssrsResponseStatus (\ s a -> s{_dssrsResponseStatus = a})

-- | A 'StreamDescriptionSummary' containing information about the stream.
dssrsStreamDescriptionSummary :: Lens' DescribeStreamSummaryResponse StreamDescriptionSummary
dssrsStreamDescriptionSummary = lens _dssrsStreamDescriptionSummary (\ s a -> s{_dssrsStreamDescriptionSummary = a})

instance NFData DescribeStreamSummaryResponse where

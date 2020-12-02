{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoMedia.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideoMedia.Types.Product where

import Network.AWS.KinesisVideoMedia.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Identifies the chunk on the Kinesis video stream where you want the @GetMedia@ API to start returning media data. You have the following options to identify the starting chunk:
--
--
--     * Choose the latest (or oldest) chunk.
--
--     * Identify a specific chunk. You can identify a specific chunk either by providing a fragment number or time stamp (server or producer).
--
--     * Each chunk's metadata includes a continuation token as a Matroska (MKV) tag (@AWS_KINESISVIDEO_CONTINUATION_TOKEN@ ). If your previous @GetMedia@ request terminated, you can use this tag value in your next @GetMedia@ request. The API then starts returning chunks starting where the last API ended.
--
--
--
--
-- /See:/ 'startSelector' smart constructor.
data StartSelector = StartSelector'
  { _ssContinuationToken   :: !(Maybe Text)
  , _ssAfterFragmentNumber :: !(Maybe Text)
  , _ssStartTimestamp      :: !(Maybe POSIX)
  , _ssStartSelectorType   :: !StartSelectorType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssContinuationToken' - Continuation token that Kinesis Video Streams returned in the previous @GetMedia@ response. The @GetMedia@ API then starts with the chunk identified by the continuation token.
--
-- * 'ssAfterFragmentNumber' - Specifies the fragment number from where you want the @GetMedia@ API to start returning the fragments.
--
-- * 'ssStartTimestamp' - A time stamp value. This value is required if you choose the PRODUCER_TIMESTAMP or the SERVER_TIMESTAMP as the @startSelectorType@ . The @GetMedia@ API then starts with the chunk containing the fragment that has the specified time stamp.
--
-- * 'ssStartSelectorType' - Identifies the fragment on the Kinesis video stream where you want to start getting the data from.     * NOW - Start with the latest chunk on the stream.     * EARLIEST - Start with earliest available chunk on the stream.     * FRAGMENT_NUMBER - Start with the chunk containing the specific fragment. You must also specify the @StartFragmentNumber@ .     * PRODUCER_TIMESTAMP or SERVER_TIMESTAMP - Start with the chunk containing a fragment with the specified producer or server time stamp. You specify the time stamp by adding @StartTimestamp@ .     * CONTINUATION_TOKEN - Read using the specified continuation token.
startSelector
    :: StartSelectorType -- ^ 'ssStartSelectorType'
    -> StartSelector
startSelector pStartSelectorType_ =
  StartSelector'
    { _ssContinuationToken = Nothing
    , _ssAfterFragmentNumber = Nothing
    , _ssStartTimestamp = Nothing
    , _ssStartSelectorType = pStartSelectorType_
    }


-- | Continuation token that Kinesis Video Streams returned in the previous @GetMedia@ response. The @GetMedia@ API then starts with the chunk identified by the continuation token.
ssContinuationToken :: Lens' StartSelector (Maybe Text)
ssContinuationToken = lens _ssContinuationToken (\ s a -> s{_ssContinuationToken = a})

-- | Specifies the fragment number from where you want the @GetMedia@ API to start returning the fragments.
ssAfterFragmentNumber :: Lens' StartSelector (Maybe Text)
ssAfterFragmentNumber = lens _ssAfterFragmentNumber (\ s a -> s{_ssAfterFragmentNumber = a})

-- | A time stamp value. This value is required if you choose the PRODUCER_TIMESTAMP or the SERVER_TIMESTAMP as the @startSelectorType@ . The @GetMedia@ API then starts with the chunk containing the fragment that has the specified time stamp.
ssStartTimestamp :: Lens' StartSelector (Maybe UTCTime)
ssStartTimestamp = lens _ssStartTimestamp (\ s a -> s{_ssStartTimestamp = a}) . mapping _Time

-- | Identifies the fragment on the Kinesis video stream where you want to start getting the data from.     * NOW - Start with the latest chunk on the stream.     * EARLIEST - Start with earliest available chunk on the stream.     * FRAGMENT_NUMBER - Start with the chunk containing the specific fragment. You must also specify the @StartFragmentNumber@ .     * PRODUCER_TIMESTAMP or SERVER_TIMESTAMP - Start with the chunk containing a fragment with the specified producer or server time stamp. You specify the time stamp by adding @StartTimestamp@ .     * CONTINUATION_TOKEN - Read using the specified continuation token.
ssStartSelectorType :: Lens' StartSelector StartSelectorType
ssStartSelectorType = lens _ssStartSelectorType (\ s a -> s{_ssStartSelectorType = a})

instance Hashable StartSelector where

instance NFData StartSelector where

instance ToJSON StartSelector where
        toJSON StartSelector'{..}
          = object
              (catMaybes
                 [("ContinuationToken" .=) <$> _ssContinuationToken,
                  ("AfterFragmentNumber" .=) <$>
                    _ssAfterFragmentNumber,
                  ("StartTimestamp" .=) <$> _ssStartTimestamp,
                  Just ("StartSelectorType" .= _ssStartSelectorType)])

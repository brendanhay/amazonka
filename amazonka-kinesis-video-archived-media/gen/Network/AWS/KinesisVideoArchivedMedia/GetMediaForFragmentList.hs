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
-- Module      : Network.AWS.KinesisVideoArchivedMedia.GetMediaForFragmentList
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets media for a list of fragments (specified by fragment number) from the archived data in a Kinesis video stream.
--
--
-- The following limits apply when using the @GetMediaForFragmentList@ API:
--
--     * A client can call @GetMediaForFragmentList@ up to five times per second per stream.
--
--     * Kinesis Video Streams sends media data at a rate of up to 25 megabytes per second (or 200 megabits per second) during a @GetMediaForFragmentList@ session.
--
--
--
module Network.AWS.KinesisVideoArchivedMedia.GetMediaForFragmentList
    (
    -- * Creating a Request
      getMediaForFragmentList
    , GetMediaForFragmentList
    -- * Request Lenses
    , gmfflStreamName
    , gmfflFragments

    -- * Destructuring the Response
    , getMediaForFragmentListResponse
    , GetMediaForFragmentListResponse
    -- * Response Lenses
    , gmfflrsContentType
    , gmfflrsResponseStatus
    , gmfflrsPayload
    ) where

import Network.AWS.KinesisVideoArchivedMedia.Types
import Network.AWS.KinesisVideoArchivedMedia.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getMediaForFragmentList' smart constructor.
data GetMediaForFragmentList = GetMediaForFragmentList'
  { _gmfflStreamName :: !Text
  , _gmfflFragments  :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMediaForFragmentList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmfflStreamName' - The name of the stream from which to retrieve fragment media.
--
-- * 'gmfflFragments' - A list of the numbers of fragments for which to retrieve media. You retrieve these values with 'ListFragments' .
getMediaForFragmentList
    :: Text -- ^ 'gmfflStreamName'
    -> GetMediaForFragmentList
getMediaForFragmentList pStreamName_ =
  GetMediaForFragmentList'
    {_gmfflStreamName = pStreamName_, _gmfflFragments = mempty}


-- | The name of the stream from which to retrieve fragment media.
gmfflStreamName :: Lens' GetMediaForFragmentList Text
gmfflStreamName = lens _gmfflStreamName (\ s a -> s{_gmfflStreamName = a})

-- | A list of the numbers of fragments for which to retrieve media. You retrieve these values with 'ListFragments' .
gmfflFragments :: Lens' GetMediaForFragmentList [Text]
gmfflFragments = lens _gmfflFragments (\ s a -> s{_gmfflFragments = a}) . _Coerce

instance AWSRequest GetMediaForFragmentList where
        type Rs GetMediaForFragmentList =
             GetMediaForFragmentListResponse
        request = postJSON kinesisVideoArchivedMedia
        response
          = receiveBody
              (\ s h x ->
                 GetMediaForFragmentListResponse' <$>
                   (h .#? "Content-Type") <*> (pure (fromEnum s)) <*>
                     (pure x))

instance Hashable GetMediaForFragmentList where

instance NFData GetMediaForFragmentList where

instance ToHeaders GetMediaForFragmentList where
        toHeaders = const mempty

instance ToJSON GetMediaForFragmentList where
        toJSON GetMediaForFragmentList'{..}
          = object
              (catMaybes
                 [Just ("StreamName" .= _gmfflStreamName),
                  Just ("Fragments" .= _gmfflFragments)])

instance ToPath GetMediaForFragmentList where
        toPath = const "/getMediaForFragmentList"

instance ToQuery GetMediaForFragmentList where
        toQuery = const mempty

-- | /See:/ 'getMediaForFragmentListResponse' smart constructor.
data GetMediaForFragmentListResponse = GetMediaForFragmentListResponse'
  { _gmfflrsContentType    :: !(Maybe Text)
  , _gmfflrsResponseStatus :: !Int
  , _gmfflrsPayload        :: !RsBody
  } deriving (Show, Generic)


-- | Creates a value of 'GetMediaForFragmentListResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmfflrsContentType' - The content type of the requested media.
--
-- * 'gmfflrsResponseStatus' - -- | The response status code.
--
-- * 'gmfflrsPayload' - The payload that Kinesis Video Streams returns is a sequence of chunks from the specified stream. For information about the chunks, see <docs.aws.amazon.com/acuity/latest/dg/API_dataplane_PutMedia.html PutMedia> . The chunks that Kinesis Video Streams returns in the @GetMediaForFragmentList@ call also include the following additional Matroska (MKV) tags:      * AWS_KINESISVIDEO_FRAGMENT_NUMBER - Fragment number returned in the chunk.     * AWS_KINESISVIDEO_SERVER_SIDE_TIMESTAMP - Server-side time stamp of the fragment.     * AWS_KINESISVIDEO_PRODUCER_SIDE_TIMESTAMP - Producer-side time stamp of the fragment. The following tags will be included if an exception occurs:     * AWS_KINESISVIDEO_FRAGMENT_NUMBER - The number of the fragment that threw the exception     * AWS_KINESISVIDEO_EXCEPTION_ERROR_CODE - The integer code of the exception     * AWS_KINESISVIDEO_EXCEPTION_MESSAGE - A text description of the exception
getMediaForFragmentListResponse
    :: Int -- ^ 'gmfflrsResponseStatus'
    -> RsBody -- ^ 'gmfflrsPayload'
    -> GetMediaForFragmentListResponse
getMediaForFragmentListResponse pResponseStatus_ pPayload_ =
  GetMediaForFragmentListResponse'
    { _gmfflrsContentType = Nothing
    , _gmfflrsResponseStatus = pResponseStatus_
    , _gmfflrsPayload = pPayload_
    }


-- | The content type of the requested media.
gmfflrsContentType :: Lens' GetMediaForFragmentListResponse (Maybe Text)
gmfflrsContentType = lens _gmfflrsContentType (\ s a -> s{_gmfflrsContentType = a})

-- | -- | The response status code.
gmfflrsResponseStatus :: Lens' GetMediaForFragmentListResponse Int
gmfflrsResponseStatus = lens _gmfflrsResponseStatus (\ s a -> s{_gmfflrsResponseStatus = a})

-- | The payload that Kinesis Video Streams returns is a sequence of chunks from the specified stream. For information about the chunks, see <docs.aws.amazon.com/acuity/latest/dg/API_dataplane_PutMedia.html PutMedia> . The chunks that Kinesis Video Streams returns in the @GetMediaForFragmentList@ call also include the following additional Matroska (MKV) tags:      * AWS_KINESISVIDEO_FRAGMENT_NUMBER - Fragment number returned in the chunk.     * AWS_KINESISVIDEO_SERVER_SIDE_TIMESTAMP - Server-side time stamp of the fragment.     * AWS_KINESISVIDEO_PRODUCER_SIDE_TIMESTAMP - Producer-side time stamp of the fragment. The following tags will be included if an exception occurs:     * AWS_KINESISVIDEO_FRAGMENT_NUMBER - The number of the fragment that threw the exception     * AWS_KINESISVIDEO_EXCEPTION_ERROR_CODE - The integer code of the exception     * AWS_KINESISVIDEO_EXCEPTION_MESSAGE - A text description of the exception
gmfflrsPayload :: Lens' GetMediaForFragmentListResponse RsBody
gmfflrsPayload = lens _gmfflrsPayload (\ s a -> s{_gmfflrsPayload = a})

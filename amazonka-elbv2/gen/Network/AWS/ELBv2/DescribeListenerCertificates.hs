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
-- Module      : Network.AWS.ELBv2.DescribeListenerCertificates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the certificates for the specified secure listener.
--
--
module Network.AWS.ELBv2.DescribeListenerCertificates
    (
    -- * Creating a Request
      describeListenerCertificates
    , DescribeListenerCertificates
    -- * Request Lenses
    , dlcMarker
    , dlcPageSize
    , dlcListenerARN

    -- * Destructuring the Response
    , describeListenerCertificatesResponse
    , DescribeListenerCertificatesResponse
    -- * Response Lenses
    , dlcrsCertificates
    , dlcrsNextMarker
    , dlcrsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeListenerCertificates' smart constructor.
data DescribeListenerCertificates = DescribeListenerCertificates'
  { _dlcMarker      :: !(Maybe Text)
  , _dlcPageSize    :: !(Maybe Nat)
  , _dlcListenerARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeListenerCertificates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlcMarker' - The marker for the next set of results. (You received this marker from a previous call.)
--
-- * 'dlcPageSize' - The maximum number of results to return with this call.
--
-- * 'dlcListenerARN' - The Amazon Resource Names (ARN) of the listener.
describeListenerCertificates
    :: Text -- ^ 'dlcListenerARN'
    -> DescribeListenerCertificates
describeListenerCertificates pListenerARN_ =
  DescribeListenerCertificates'
    { _dlcMarker = Nothing
    , _dlcPageSize = Nothing
    , _dlcListenerARN = pListenerARN_
    }


-- | The marker for the next set of results. (You received this marker from a previous call.)
dlcMarker :: Lens' DescribeListenerCertificates (Maybe Text)
dlcMarker = lens _dlcMarker (\ s a -> s{_dlcMarker = a})

-- | The maximum number of results to return with this call.
dlcPageSize :: Lens' DescribeListenerCertificates (Maybe Natural)
dlcPageSize = lens _dlcPageSize (\ s a -> s{_dlcPageSize = a}) . mapping _Nat

-- | The Amazon Resource Names (ARN) of the listener.
dlcListenerARN :: Lens' DescribeListenerCertificates Text
dlcListenerARN = lens _dlcListenerARN (\ s a -> s{_dlcListenerARN = a})

instance AWSRequest DescribeListenerCertificates
         where
        type Rs DescribeListenerCertificates =
             DescribeListenerCertificatesResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper
              "DescribeListenerCertificatesResult"
              (\ s h x ->
                 DescribeListenerCertificatesResponse' <$>
                   (x .@? "Certificates" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeListenerCertificates where

instance NFData DescribeListenerCertificates where

instance ToHeaders DescribeListenerCertificates where
        toHeaders = const mempty

instance ToPath DescribeListenerCertificates where
        toPath = const "/"

instance ToQuery DescribeListenerCertificates where
        toQuery DescribeListenerCertificates'{..}
          = mconcat
              ["Action" =:
                 ("DescribeListenerCertificates" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "Marker" =: _dlcMarker, "PageSize" =: _dlcPageSize,
               "ListenerArn" =: _dlcListenerARN]

-- | /See:/ 'describeListenerCertificatesResponse' smart constructor.
data DescribeListenerCertificatesResponse = DescribeListenerCertificatesResponse'
  { _dlcrsCertificates   :: !(Maybe [Certificate])
  , _dlcrsNextMarker     :: !(Maybe Text)
  , _dlcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeListenerCertificatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlcrsCertificates' - Information about the certificates.
--
-- * 'dlcrsNextMarker' - The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- * 'dlcrsResponseStatus' - -- | The response status code.
describeListenerCertificatesResponse
    :: Int -- ^ 'dlcrsResponseStatus'
    -> DescribeListenerCertificatesResponse
describeListenerCertificatesResponse pResponseStatus_ =
  DescribeListenerCertificatesResponse'
    { _dlcrsCertificates = Nothing
    , _dlcrsNextMarker = Nothing
    , _dlcrsResponseStatus = pResponseStatus_
    }


-- | Information about the certificates.
dlcrsCertificates :: Lens' DescribeListenerCertificatesResponse [Certificate]
dlcrsCertificates = lens _dlcrsCertificates (\ s a -> s{_dlcrsCertificates = a}) . _Default . _Coerce

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
dlcrsNextMarker :: Lens' DescribeListenerCertificatesResponse (Maybe Text)
dlcrsNextMarker = lens _dlcrsNextMarker (\ s a -> s{_dlcrsNextMarker = a})

-- | -- | The response status code.
dlcrsResponseStatus :: Lens' DescribeListenerCertificatesResponse Int
dlcrsResponseStatus = lens _dlcrsResponseStatus (\ s a -> s{_dlcrsResponseStatus = a})

instance NFData DescribeListenerCertificatesResponse
         where

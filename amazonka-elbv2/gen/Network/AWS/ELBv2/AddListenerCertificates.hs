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
-- Module      : Network.AWS.ELBv2.AddListenerCertificates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified certificate to the specified secure listener.
--
--
-- If the certificate was already added, the call is successful but the certificate is not added again.
--
-- To list the certificates for your listener, use 'DescribeListenerCertificates' . To remove certificates from your listener, use 'RemoveListenerCertificates' .
--
module Network.AWS.ELBv2.AddListenerCertificates
    (
    -- * Creating a Request
      addListenerCertificates
    , AddListenerCertificates
    -- * Request Lenses
    , alcListenerARN
    , alcCertificates

    -- * Destructuring the Response
    , addListenerCertificatesResponse
    , AddListenerCertificatesResponse
    -- * Response Lenses
    , alcrsCertificates
    , alcrsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'addListenerCertificates' smart constructor.
data AddListenerCertificates = AddListenerCertificates'
  { _alcListenerARN  :: !Text
  , _alcCertificates :: ![Certificate]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddListenerCertificates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alcListenerARN' - The Amazon Resource Name (ARN) of the listener.
--
-- * 'alcCertificates' - The certificate to add. You can specify one certificate per call.
addListenerCertificates
    :: Text -- ^ 'alcListenerARN'
    -> AddListenerCertificates
addListenerCertificates pListenerARN_ =
  AddListenerCertificates'
    {_alcListenerARN = pListenerARN_, _alcCertificates = mempty}


-- | The Amazon Resource Name (ARN) of the listener.
alcListenerARN :: Lens' AddListenerCertificates Text
alcListenerARN = lens _alcListenerARN (\ s a -> s{_alcListenerARN = a})

-- | The certificate to add. You can specify one certificate per call.
alcCertificates :: Lens' AddListenerCertificates [Certificate]
alcCertificates = lens _alcCertificates (\ s a -> s{_alcCertificates = a}) . _Coerce

instance AWSRequest AddListenerCertificates where
        type Rs AddListenerCertificates =
             AddListenerCertificatesResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper "AddListenerCertificatesResult"
              (\ s h x ->
                 AddListenerCertificatesResponse' <$>
                   (x .@? "Certificates" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable AddListenerCertificates where

instance NFData AddListenerCertificates where

instance ToHeaders AddListenerCertificates where
        toHeaders = const mempty

instance ToPath AddListenerCertificates where
        toPath = const "/"

instance ToQuery AddListenerCertificates where
        toQuery AddListenerCertificates'{..}
          = mconcat
              ["Action" =:
                 ("AddListenerCertificates" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "ListenerArn" =: _alcListenerARN,
               "Certificates" =:
                 toQueryList "member" _alcCertificates]

-- | /See:/ 'addListenerCertificatesResponse' smart constructor.
data AddListenerCertificatesResponse = AddListenerCertificatesResponse'
  { _alcrsCertificates   :: !(Maybe [Certificate])
  , _alcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddListenerCertificatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alcrsCertificates' - Information about the certificates.
--
-- * 'alcrsResponseStatus' - -- | The response status code.
addListenerCertificatesResponse
    :: Int -- ^ 'alcrsResponseStatus'
    -> AddListenerCertificatesResponse
addListenerCertificatesResponse pResponseStatus_ =
  AddListenerCertificatesResponse'
    {_alcrsCertificates = Nothing, _alcrsResponseStatus = pResponseStatus_}


-- | Information about the certificates.
alcrsCertificates :: Lens' AddListenerCertificatesResponse [Certificate]
alcrsCertificates = lens _alcrsCertificates (\ s a -> s{_alcrsCertificates = a}) . _Default . _Coerce

-- | -- | The response status code.
alcrsResponseStatus :: Lens' AddListenerCertificatesResponse Int
alcrsResponseStatus = lens _alcrsResponseStatus (\ s a -> s{_alcrsResponseStatus = a})

instance NFData AddListenerCertificatesResponse where

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
-- Module      : Network.AWS.ELBv2.RemoveListenerCertificates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified certificate from the specified secure listener.
--
--
-- You can't remove the default certificate for a listener. To replace the default certificate, call 'ModifyListener' .
--
-- To list the certificates for your listener, use 'DescribeListenerCertificates' .
--
module Network.AWS.ELBv2.RemoveListenerCertificates
    (
    -- * Creating a Request
      removeListenerCertificates
    , RemoveListenerCertificates
    -- * Request Lenses
    , rlcListenerARN
    , rlcCertificates

    -- * Destructuring the Response
    , removeListenerCertificatesResponse
    , RemoveListenerCertificatesResponse
    -- * Response Lenses
    , rlcrsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeListenerCertificates' smart constructor.
data RemoveListenerCertificates = RemoveListenerCertificates'
  { _rlcListenerARN  :: !Text
  , _rlcCertificates :: ![Certificate]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveListenerCertificates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rlcListenerARN' - The Amazon Resource Name (ARN) of the listener.
--
-- * 'rlcCertificates' - The certificate to remove. You can specify one certificate per call.
removeListenerCertificates
    :: Text -- ^ 'rlcListenerARN'
    -> RemoveListenerCertificates
removeListenerCertificates pListenerARN_ =
  RemoveListenerCertificates'
    {_rlcListenerARN = pListenerARN_, _rlcCertificates = mempty}


-- | The Amazon Resource Name (ARN) of the listener.
rlcListenerARN :: Lens' RemoveListenerCertificates Text
rlcListenerARN = lens _rlcListenerARN (\ s a -> s{_rlcListenerARN = a})

-- | The certificate to remove. You can specify one certificate per call.
rlcCertificates :: Lens' RemoveListenerCertificates [Certificate]
rlcCertificates = lens _rlcCertificates (\ s a -> s{_rlcCertificates = a}) . _Coerce

instance AWSRequest RemoveListenerCertificates where
        type Rs RemoveListenerCertificates =
             RemoveListenerCertificatesResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper
              "RemoveListenerCertificatesResult"
              (\ s h x ->
                 RemoveListenerCertificatesResponse' <$>
                   (pure (fromEnum s)))

instance Hashable RemoveListenerCertificates where

instance NFData RemoveListenerCertificates where

instance ToHeaders RemoveListenerCertificates where
        toHeaders = const mempty

instance ToPath RemoveListenerCertificates where
        toPath = const "/"

instance ToQuery RemoveListenerCertificates where
        toQuery RemoveListenerCertificates'{..}
          = mconcat
              ["Action" =:
                 ("RemoveListenerCertificates" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "ListenerArn" =: _rlcListenerARN,
               "Certificates" =:
                 toQueryList "member" _rlcCertificates]

-- | /See:/ 'removeListenerCertificatesResponse' smart constructor.
newtype RemoveListenerCertificatesResponse = RemoveListenerCertificatesResponse'
  { _rlcrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveListenerCertificatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rlcrsResponseStatus' - -- | The response status code.
removeListenerCertificatesResponse
    :: Int -- ^ 'rlcrsResponseStatus'
    -> RemoveListenerCertificatesResponse
removeListenerCertificatesResponse pResponseStatus_ =
  RemoveListenerCertificatesResponse' {_rlcrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
rlcrsResponseStatus :: Lens' RemoveListenerCertificatesResponse Int
rlcrsResponseStatus = lens _rlcrsResponseStatus (\ s a -> s{_rlcrsResponseStatus = a})

instance NFData RemoveListenerCertificatesResponse
         where

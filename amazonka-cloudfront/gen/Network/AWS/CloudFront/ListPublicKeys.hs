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
-- Module      : Network.AWS.CloudFront.ListPublicKeys
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all public keys that have been added to CloudFront for this account.
--
--
module Network.AWS.CloudFront.ListPublicKeys
    (
    -- * Creating a Request
      listPublicKeys
    , ListPublicKeys
    -- * Request Lenses
    , lpkMarker
    , lpkMaxItems

    -- * Destructuring the Response
    , listPublicKeysResponse
    , ListPublicKeysResponse
    -- * Response Lenses
    , lpkrsPublicKeyList
    , lpkrsResponseStatus
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listPublicKeys' smart constructor.
data ListPublicKeys = ListPublicKeys'
  { _lpkMarker   :: !(Maybe Text)
  , _lpkMaxItems :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPublicKeys' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpkMarker' - Use this when paginating results to indicate where to begin in your list of public keys. The results include public keys in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last public key on that page).
--
-- * 'lpkMaxItems' - The maximum number of public keys you want in the response body.
listPublicKeys
    :: ListPublicKeys
listPublicKeys = ListPublicKeys' {_lpkMarker = Nothing, _lpkMaxItems = Nothing}


-- | Use this when paginating results to indicate where to begin in your list of public keys. The results include public keys in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last public key on that page).
lpkMarker :: Lens' ListPublicKeys (Maybe Text)
lpkMarker = lens _lpkMarker (\ s a -> s{_lpkMarker = a})

-- | The maximum number of public keys you want in the response body.
lpkMaxItems :: Lens' ListPublicKeys (Maybe Text)
lpkMaxItems = lens _lpkMaxItems (\ s a -> s{_lpkMaxItems = a})

instance AWSRequest ListPublicKeys where
        type Rs ListPublicKeys = ListPublicKeysResponse
        request = get cloudFront
        response
          = receiveXML
              (\ s h x ->
                 ListPublicKeysResponse' <$>
                   (parseXML x) <*> (pure (fromEnum s)))

instance Hashable ListPublicKeys where

instance NFData ListPublicKeys where

instance ToHeaders ListPublicKeys where
        toHeaders = const mempty

instance ToPath ListPublicKeys where
        toPath = const "/2017-10-30/public-key"

instance ToQuery ListPublicKeys where
        toQuery ListPublicKeys'{..}
          = mconcat
              ["Marker" =: _lpkMarker, "MaxItems" =: _lpkMaxItems]

-- | /See:/ 'listPublicKeysResponse' smart constructor.
data ListPublicKeysResponse = ListPublicKeysResponse'
  { _lpkrsPublicKeyList  :: !(Maybe PublicKeyList)
  , _lpkrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPublicKeysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpkrsPublicKeyList' - Returns a list of all public keys that have been added to CloudFront for this account.
--
-- * 'lpkrsResponseStatus' - -- | The response status code.
listPublicKeysResponse
    :: Int -- ^ 'lpkrsResponseStatus'
    -> ListPublicKeysResponse
listPublicKeysResponse pResponseStatus_ =
  ListPublicKeysResponse'
    {_lpkrsPublicKeyList = Nothing, _lpkrsResponseStatus = pResponseStatus_}


-- | Returns a list of all public keys that have been added to CloudFront for this account.
lpkrsPublicKeyList :: Lens' ListPublicKeysResponse (Maybe PublicKeyList)
lpkrsPublicKeyList = lens _lpkrsPublicKeyList (\ s a -> s{_lpkrsPublicKeyList = a})

-- | -- | The response status code.
lpkrsResponseStatus :: Lens' ListPublicKeysResponse Int
lpkrsResponseStatus = lens _lpkrsResponseStatus (\ s a -> s{_lpkrsResponseStatus = a})

instance NFData ListPublicKeysResponse where

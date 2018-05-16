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
-- Module      : Network.AWS.StorageGateway.ListFileShares
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the file shares for a specific file gateway, or the list of file shares that belong to the calling user account. This operation is only supported in the file gateway type.
--
--
module Network.AWS.StorageGateway.ListFileShares
    (
    -- * Creating a Request
      listFileShares
    , ListFileShares
    -- * Request Lenses
    , lfsGatewayARN
    , lfsMarker
    , lfsLimit

    -- * Destructuring the Response
    , listFileSharesResponse
    , ListFileSharesResponse
    -- * Response Lenses
    , lfsrsFileShareInfoList
    , lfsrsMarker
    , lfsrsNextMarker
    , lfsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | ListFileShareInput
--
--
--
-- /See:/ 'listFileShares' smart constructor.
data ListFileShares = ListFileShares'
  { _lfsGatewayARN :: !(Maybe Text)
  , _lfsMarker     :: !(Maybe Text)
  , _lfsLimit      :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListFileShares' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfsGatewayARN' - The Amazon resource Name (ARN) of the gateway whose file shares you want to list. If this field is not present, all file shares under your account are listed.
--
-- * 'lfsMarker' - Opaque pagination token returned from a previous ListFileShares operation. If present, @Marker@ specifies where to continue the list from after a previous call to ListFileShares. Optional.
--
-- * 'lfsLimit' - The maximum number of file shares to return in the response. The value must be an integer with a value greater than zero. Optional.
listFileShares
    :: ListFileShares
listFileShares =
  ListFileShares'
    {_lfsGatewayARN = Nothing, _lfsMarker = Nothing, _lfsLimit = Nothing}


-- | The Amazon resource Name (ARN) of the gateway whose file shares you want to list. If this field is not present, all file shares under your account are listed.
lfsGatewayARN :: Lens' ListFileShares (Maybe Text)
lfsGatewayARN = lens _lfsGatewayARN (\ s a -> s{_lfsGatewayARN = a})

-- | Opaque pagination token returned from a previous ListFileShares operation. If present, @Marker@ specifies where to continue the list from after a previous call to ListFileShares. Optional.
lfsMarker :: Lens' ListFileShares (Maybe Text)
lfsMarker = lens _lfsMarker (\ s a -> s{_lfsMarker = a})

-- | The maximum number of file shares to return in the response. The value must be an integer with a value greater than zero. Optional.
lfsLimit :: Lens' ListFileShares (Maybe Natural)
lfsLimit = lens _lfsLimit (\ s a -> s{_lfsLimit = a}) . mapping _Nat

instance AWSRequest ListFileShares where
        type Rs ListFileShares = ListFileSharesResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 ListFileSharesResponse' <$>
                   (x .?> "FileShareInfoList" .!@ mempty) <*>
                     (x .?> "Marker")
                     <*> (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListFileShares where

instance NFData ListFileShares where

instance ToHeaders ListFileShares where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.ListFileShares" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListFileShares where
        toJSON ListFileShares'{..}
          = object
              (catMaybes
                 [("GatewayARN" .=) <$> _lfsGatewayARN,
                  ("Marker" .=) <$> _lfsMarker,
                  ("Limit" .=) <$> _lfsLimit])

instance ToPath ListFileShares where
        toPath = const "/"

instance ToQuery ListFileShares where
        toQuery = const mempty

-- | ListFileShareOutput
--
--
--
-- /See:/ 'listFileSharesResponse' smart constructor.
data ListFileSharesResponse = ListFileSharesResponse'
  { _lfsrsFileShareInfoList :: !(Maybe [FileShareInfo])
  , _lfsrsMarker            :: !(Maybe Text)
  , _lfsrsNextMarker        :: !(Maybe Text)
  , _lfsrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListFileSharesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfsrsFileShareInfoList' - An array of information about the file gateway's file shares.
--
-- * 'lfsrsMarker' - If the request includes @Marker@ , the response returns that value in this field.
--
-- * 'lfsrsNextMarker' - If a value is present, there are more file shares to return. In a subsequent request, use @NextMarker@ as the value for @Marker@ to retrieve the next set of file shares.
--
-- * 'lfsrsResponseStatus' - -- | The response status code.
listFileSharesResponse
    :: Int -- ^ 'lfsrsResponseStatus'
    -> ListFileSharesResponse
listFileSharesResponse pResponseStatus_ =
  ListFileSharesResponse'
    { _lfsrsFileShareInfoList = Nothing
    , _lfsrsMarker = Nothing
    , _lfsrsNextMarker = Nothing
    , _lfsrsResponseStatus = pResponseStatus_
    }


-- | An array of information about the file gateway's file shares.
lfsrsFileShareInfoList :: Lens' ListFileSharesResponse [FileShareInfo]
lfsrsFileShareInfoList = lens _lfsrsFileShareInfoList (\ s a -> s{_lfsrsFileShareInfoList = a}) . _Default . _Coerce

-- | If the request includes @Marker@ , the response returns that value in this field.
lfsrsMarker :: Lens' ListFileSharesResponse (Maybe Text)
lfsrsMarker = lens _lfsrsMarker (\ s a -> s{_lfsrsMarker = a})

-- | If a value is present, there are more file shares to return. In a subsequent request, use @NextMarker@ as the value for @Marker@ to retrieve the next set of file shares.
lfsrsNextMarker :: Lens' ListFileSharesResponse (Maybe Text)
lfsrsNextMarker = lens _lfsrsNextMarker (\ s a -> s{_lfsrsNextMarker = a})

-- | -- | The response status code.
lfsrsResponseStatus :: Lens' ListFileSharesResponse Int
lfsrsResponseStatus = lens _lfsrsResponseStatus (\ s a -> s{_lfsrsResponseStatus = a})

instance NFData ListFileSharesResponse where

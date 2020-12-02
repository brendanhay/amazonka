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
-- Module      : Network.AWS.APIGateway.GetSDKTypes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.GetSDKTypes
    (
    -- * Creating a Request
      getSDKTypes
    , GetSDKTypes
    -- * Request Lenses
    , gstLimit
    , gstPosition

    -- * Destructuring the Response
    , getSDKTypesResponse
    , GetSDKTypesResponse
    -- * Response Lenses
    , gstrsItems
    , gstrsPosition
    , gstrsResponseStatus
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Get the 'SdkTypes' collection.
--
--
--
-- /See:/ 'getSDKTypes' smart constructor.
data GetSDKTypes = GetSDKTypes'
  { _gstLimit    :: !(Maybe Int)
  , _gstPosition :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSDKTypes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gstLimit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- * 'gstPosition' - The current pagination position in the paged result set.
getSDKTypes
    :: GetSDKTypes
getSDKTypes = GetSDKTypes' {_gstLimit = Nothing, _gstPosition = Nothing}


-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
gstLimit :: Lens' GetSDKTypes (Maybe Int)
gstLimit = lens _gstLimit (\ s a -> s{_gstLimit = a})

-- | The current pagination position in the paged result set.
gstPosition :: Lens' GetSDKTypes (Maybe Text)
gstPosition = lens _gstPosition (\ s a -> s{_gstPosition = a})

instance AWSRequest GetSDKTypes where
        type Rs GetSDKTypes = GetSDKTypesResponse
        request = get apiGateway
        response
          = receiveJSON
              (\ s h x ->
                 GetSDKTypesResponse' <$>
                   (x .?> "item" .!@ mempty) <*> (x .?> "position") <*>
                     (pure (fromEnum s)))

instance Hashable GetSDKTypes where

instance NFData GetSDKTypes where

instance ToHeaders GetSDKTypes where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetSDKTypes where
        toPath = const "/sdktypes"

instance ToQuery GetSDKTypes where
        toQuery GetSDKTypes'{..}
          = mconcat
              ["limit" =: _gstLimit, "position" =: _gstPosition]

-- | The collection of 'SdkType' instances.
--
--
--
-- /See:/ 'getSDKTypesResponse' smart constructor.
data GetSDKTypesResponse = GetSDKTypesResponse'
  { _gstrsItems          :: !(Maybe [SDKType])
  , _gstrsPosition       :: !(Maybe Text)
  , _gstrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSDKTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gstrsItems' - The current page of elements from this collection.
--
-- * 'gstrsPosition' - Undocumented member.
--
-- * 'gstrsResponseStatus' - -- | The response status code.
getSDKTypesResponse
    :: Int -- ^ 'gstrsResponseStatus'
    -> GetSDKTypesResponse
getSDKTypesResponse pResponseStatus_ =
  GetSDKTypesResponse'
    { _gstrsItems = Nothing
    , _gstrsPosition = Nothing
    , _gstrsResponseStatus = pResponseStatus_
    }


-- | The current page of elements from this collection.
gstrsItems :: Lens' GetSDKTypesResponse [SDKType]
gstrsItems = lens _gstrsItems (\ s a -> s{_gstrsItems = a}) . _Default . _Coerce

-- | Undocumented member.
gstrsPosition :: Lens' GetSDKTypesResponse (Maybe Text)
gstrsPosition = lens _gstrsPosition (\ s a -> s{_gstrsPosition = a})

-- | -- | The response status code.
gstrsResponseStatus :: Lens' GetSDKTypesResponse Int
gstrsResponseStatus = lens _gstrsResponseStatus (\ s a -> s{_gstrsResponseStatus = a})

instance NFData GetSDKTypesResponse where

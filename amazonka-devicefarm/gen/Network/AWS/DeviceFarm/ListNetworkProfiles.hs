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
-- Module      : Network.AWS.DeviceFarm.ListNetworkProfiles
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of available network profiles.
--
--
module Network.AWS.DeviceFarm.ListNetworkProfiles
    (
    -- * Creating a Request
      listNetworkProfiles
    , ListNetworkProfiles
    -- * Request Lenses
    , lnpNextToken
    , lnpType
    , lnpArn

    -- * Destructuring the Response
    , listNetworkProfilesResponse
    , ListNetworkProfilesResponse
    -- * Response Lenses
    , lnprsNetworkProfiles
    , lnprsNextToken
    , lnprsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listNetworkProfiles' smart constructor.
data ListNetworkProfiles = ListNetworkProfiles'
  { _lnpNextToken :: !(Maybe Text)
  , _lnpType      :: !(Maybe NetworkProfileType)
  , _lnpArn       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListNetworkProfiles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lnpNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'lnpType' - The type of network profile you wish to return information about. Valid values are listed below.
--
-- * 'lnpArn' - The Amazon Resource Name (ARN) of the project for which you want to list network profiles.
listNetworkProfiles
    :: Text -- ^ 'lnpArn'
    -> ListNetworkProfiles
listNetworkProfiles pArn_ =
  ListNetworkProfiles'
    {_lnpNextToken = Nothing, _lnpType = Nothing, _lnpArn = pArn_}


-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
lnpNextToken :: Lens' ListNetworkProfiles (Maybe Text)
lnpNextToken = lens _lnpNextToken (\ s a -> s{_lnpNextToken = a})

-- | The type of network profile you wish to return information about. Valid values are listed below.
lnpType :: Lens' ListNetworkProfiles (Maybe NetworkProfileType)
lnpType = lens _lnpType (\ s a -> s{_lnpType = a})

-- | The Amazon Resource Name (ARN) of the project for which you want to list network profiles.
lnpArn :: Lens' ListNetworkProfiles Text
lnpArn = lens _lnpArn (\ s a -> s{_lnpArn = a})

instance AWSRequest ListNetworkProfiles where
        type Rs ListNetworkProfiles =
             ListNetworkProfilesResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 ListNetworkProfilesResponse' <$>
                   (x .?> "networkProfiles" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListNetworkProfiles where

instance NFData ListNetworkProfiles where

instance ToHeaders ListNetworkProfiles where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.ListNetworkProfiles" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListNetworkProfiles where
        toJSON ListNetworkProfiles'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _lnpNextToken,
                  ("type" .=) <$> _lnpType, Just ("arn" .= _lnpArn)])

instance ToPath ListNetworkProfiles where
        toPath = const "/"

instance ToQuery ListNetworkProfiles where
        toQuery = const mempty

-- | /See:/ 'listNetworkProfilesResponse' smart constructor.
data ListNetworkProfilesResponse = ListNetworkProfilesResponse'
  { _lnprsNetworkProfiles :: !(Maybe [NetworkProfile])
  , _lnprsNextToken       :: !(Maybe Text)
  , _lnprsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListNetworkProfilesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lnprsNetworkProfiles' - A list of the available network profiles.
--
-- * 'lnprsNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'lnprsResponseStatus' - -- | The response status code.
listNetworkProfilesResponse
    :: Int -- ^ 'lnprsResponseStatus'
    -> ListNetworkProfilesResponse
listNetworkProfilesResponse pResponseStatus_ =
  ListNetworkProfilesResponse'
    { _lnprsNetworkProfiles = Nothing
    , _lnprsNextToken = Nothing
    , _lnprsResponseStatus = pResponseStatus_
    }


-- | A list of the available network profiles.
lnprsNetworkProfiles :: Lens' ListNetworkProfilesResponse [NetworkProfile]
lnprsNetworkProfiles = lens _lnprsNetworkProfiles (\ s a -> s{_lnprsNetworkProfiles = a}) . _Default . _Coerce

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
lnprsNextToken :: Lens' ListNetworkProfilesResponse (Maybe Text)
lnprsNextToken = lens _lnprsNextToken (\ s a -> s{_lnprsNextToken = a})

-- | -- | The response status code.
lnprsResponseStatus :: Lens' ListNetworkProfilesResponse Int
lnprsResponseStatus = lens _lnprsResponseStatus (\ s a -> s{_lnprsResponseStatus = a})

instance NFData ListNetworkProfilesResponse where

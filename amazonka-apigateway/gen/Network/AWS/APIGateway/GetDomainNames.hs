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
-- Module      : Network.AWS.APIGateway.GetDomainNames
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a collection of < DomainName> resources.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetDomainNames
    (
    -- * Creating a Request
      getDomainNames
    , GetDomainNames
    -- * Request Lenses
    , gdnLimit
    , gdnPosition

    -- * Destructuring the Response
    , getDomainNamesResponse
    , GetDomainNamesResponse
    -- * Response Lenses
    , gdnrsItems
    , gdnrsPosition
    , gdnrsResponseStatus
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Request to describe a collection of < DomainName> resources.
--
-- /See:/ 'getDomainNames' smart constructor.
data GetDomainNames = GetDomainNames'
    { _gdnLimit    :: !(Maybe Int)
    , _gdnPosition :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetDomainNames' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdnLimit'
--
-- * 'gdnPosition'
getDomainNames
    :: GetDomainNames
getDomainNames =
    GetDomainNames'
    { _gdnLimit = Nothing
    , _gdnPosition = Nothing
    }

-- | The maximum number of < DomainName> resources in the collection to get information about. The default limit is 25. It should be an integer between 1 - 500.
gdnLimit :: Lens' GetDomainNames (Maybe Int)
gdnLimit = lens _gdnLimit (\ s a -> s{_gdnLimit = a});

-- | The position of the current domain names to get information about.
gdnPosition :: Lens' GetDomainNames (Maybe Text)
gdnPosition = lens _gdnPosition (\ s a -> s{_gdnPosition = a});

instance AWSPager GetDomainNames where
        page rq rs
          | stop (rs ^. gdnrsPosition) = Nothing
          | stop (rs ^. gdnrsItems) = Nothing
          | otherwise =
            Just $ rq & gdnPosition .~ rs ^. gdnrsPosition

instance AWSRequest GetDomainNames where
        type Rs GetDomainNames = GetDomainNamesResponse
        request = get apiGateway
        response
          = receiveJSON
              (\ s h x ->
                 GetDomainNamesResponse' <$>
                   (x .?> "item" .!@ mempty) <*> (x .?> "position") <*>
                     (pure (fromEnum s)))

instance Hashable GetDomainNames

instance NFData GetDomainNames

instance ToHeaders GetDomainNames where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetDomainNames where
        toPath = const "/domainnames"

instance ToQuery GetDomainNames where
        toQuery GetDomainNames'{..}
          = mconcat
              ["limit" =: _gdnLimit, "position" =: _gdnPosition]

-- | Represents a collection of < DomainName> resources.
--
-- /See:/ 'getDomainNamesResponse' smart constructor.
data GetDomainNamesResponse = GetDomainNamesResponse'
    { _gdnrsItems          :: !(Maybe [DomainName])
    , _gdnrsPosition       :: !(Maybe Text)
    , _gdnrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetDomainNamesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdnrsItems'
--
-- * 'gdnrsPosition'
--
-- * 'gdnrsResponseStatus'
getDomainNamesResponse
    :: Int -- ^ 'gdnrsResponseStatus'
    -> GetDomainNamesResponse
getDomainNamesResponse pResponseStatus_ =
    GetDomainNamesResponse'
    { _gdnrsItems = Nothing
    , _gdnrsPosition = Nothing
    , _gdnrsResponseStatus = pResponseStatus_
    }

-- | The current page of any < DomainName> resources in the collection of < DomainName> resources.
gdnrsItems :: Lens' GetDomainNamesResponse [DomainName]
gdnrsItems = lens _gdnrsItems (\ s a -> s{_gdnrsItems = a}) . _Default . _Coerce;

-- | Undocumented member.
gdnrsPosition :: Lens' GetDomainNamesResponse (Maybe Text)
gdnrsPosition = lens _gdnrsPosition (\ s a -> s{_gdnrsPosition = a});

-- | The response status code.
gdnrsResponseStatus :: Lens' GetDomainNamesResponse Int
gdnrsResponseStatus = lens _gdnrsResponseStatus (\ s a -> s{_gdnrsResponseStatus = a});

instance NFData GetDomainNamesResponse

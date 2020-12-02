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
-- Module      : Network.AWS.Lightsail.GetBlueprints
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of available instance images, or /blueprints/ . You can use a blueprint to create a new virtual private server already running a specific operating system, as well as a preinstalled app or development stack. The software each instance is running depends on the blueprint image you choose.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetBlueprints
    (
    -- * Creating a Request
      getBlueprints
    , GetBlueprints
    -- * Request Lenses
    , gbIncludeInactive
    , gbPageToken

    -- * Destructuring the Response
    , getBlueprintsResponse
    , GetBlueprintsResponse
    -- * Response Lenses
    , gbsrsBlueprints
    , gbsrsNextPageToken
    , gbsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getBlueprints' smart constructor.
data GetBlueprints = GetBlueprints'
  { _gbIncludeInactive :: !(Maybe Bool)
  , _gbPageToken       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBlueprints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbIncludeInactive' - A Boolean value indicating whether to include inactive results in your request.
--
-- * 'gbPageToken' - A token used for advancing to the next page of results from your get blueprints request.
getBlueprints
    :: GetBlueprints
getBlueprints =
  GetBlueprints' {_gbIncludeInactive = Nothing, _gbPageToken = Nothing}


-- | A Boolean value indicating whether to include inactive results in your request.
gbIncludeInactive :: Lens' GetBlueprints (Maybe Bool)
gbIncludeInactive = lens _gbIncludeInactive (\ s a -> s{_gbIncludeInactive = a})

-- | A token used for advancing to the next page of results from your get blueprints request.
gbPageToken :: Lens' GetBlueprints (Maybe Text)
gbPageToken = lens _gbPageToken (\ s a -> s{_gbPageToken = a})

instance AWSPager GetBlueprints where
        page rq rs
          | stop (rs ^. gbsrsNextPageToken) = Nothing
          | stop (rs ^. gbsrsBlueprints) = Nothing
          | otherwise =
            Just $ rq & gbPageToken .~ rs ^. gbsrsNextPageToken

instance AWSRequest GetBlueprints where
        type Rs GetBlueprints = GetBlueprintsResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetBlueprintsResponse' <$>
                   (x .?> "blueprints" .!@ mempty) <*>
                     (x .?> "nextPageToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetBlueprints where

instance NFData GetBlueprints where

instance ToHeaders GetBlueprints where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetBlueprints" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetBlueprints where
        toJSON GetBlueprints'{..}
          = object
              (catMaybes
                 [("includeInactive" .=) <$> _gbIncludeInactive,
                  ("pageToken" .=) <$> _gbPageToken])

instance ToPath GetBlueprints where
        toPath = const "/"

instance ToQuery GetBlueprints where
        toQuery = const mempty

-- | /See:/ 'getBlueprintsResponse' smart constructor.
data GetBlueprintsResponse = GetBlueprintsResponse'
  { _gbsrsBlueprints     :: !(Maybe [Blueprint])
  , _gbsrsNextPageToken  :: !(Maybe Text)
  , _gbsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBlueprintsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbsrsBlueprints' - An array of key-value pairs that contains information about the available blueprints.
--
-- * 'gbsrsNextPageToken' - A token used for advancing to the next page of results from your get blueprints request.
--
-- * 'gbsrsResponseStatus' - -- | The response status code.
getBlueprintsResponse
    :: Int -- ^ 'gbsrsResponseStatus'
    -> GetBlueprintsResponse
getBlueprintsResponse pResponseStatus_ =
  GetBlueprintsResponse'
    { _gbsrsBlueprints = Nothing
    , _gbsrsNextPageToken = Nothing
    , _gbsrsResponseStatus = pResponseStatus_
    }


-- | An array of key-value pairs that contains information about the available blueprints.
gbsrsBlueprints :: Lens' GetBlueprintsResponse [Blueprint]
gbsrsBlueprints = lens _gbsrsBlueprints (\ s a -> s{_gbsrsBlueprints = a}) . _Default . _Coerce

-- | A token used for advancing to the next page of results from your get blueprints request.
gbsrsNextPageToken :: Lens' GetBlueprintsResponse (Maybe Text)
gbsrsNextPageToken = lens _gbsrsNextPageToken (\ s a -> s{_gbsrsNextPageToken = a})

-- | -- | The response status code.
gbsrsResponseStatus :: Lens' GetBlueprintsResponse Int
gbsrsResponseStatus = lens _gbsrsResponseStatus (\ s a -> s{_gbsrsResponseStatus = a})

instance NFData GetBlueprintsResponse where

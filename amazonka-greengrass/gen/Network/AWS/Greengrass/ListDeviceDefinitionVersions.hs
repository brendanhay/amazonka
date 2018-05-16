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
-- Module      : Network.AWS.Greengrass.ListDeviceDefinitionVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a device definition.
module Network.AWS.Greengrass.ListDeviceDefinitionVersions
    (
    -- * Creating a Request
      listDeviceDefinitionVersions
    , ListDeviceDefinitionVersions
    -- * Request Lenses
    , lddvNextToken
    , lddvMaxResults
    , lddvDeviceDefinitionId

    -- * Destructuring the Response
    , listDeviceDefinitionVersionsResponse
    , ListDeviceDefinitionVersionsResponse
    -- * Response Lenses
    , lddvrsVersions
    , lddvrsNextToken
    , lddvrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDeviceDefinitionVersions' smart constructor.
data ListDeviceDefinitionVersions = ListDeviceDefinitionVersions'
  { _lddvNextToken          :: !(Maybe Text)
  , _lddvMaxResults         :: !(Maybe Text)
  , _lddvDeviceDefinitionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeviceDefinitionVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lddvNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lddvMaxResults' - The maximum number of results to be returned per request.
--
-- * 'lddvDeviceDefinitionId' - The ID of the device definition.
listDeviceDefinitionVersions
    :: Text -- ^ 'lddvDeviceDefinitionId'
    -> ListDeviceDefinitionVersions
listDeviceDefinitionVersions pDeviceDefinitionId_ =
  ListDeviceDefinitionVersions'
    { _lddvNextToken = Nothing
    , _lddvMaxResults = Nothing
    , _lddvDeviceDefinitionId = pDeviceDefinitionId_
    }


-- | The token for the next set of results, or ''null'' if there are no additional results.
lddvNextToken :: Lens' ListDeviceDefinitionVersions (Maybe Text)
lddvNextToken = lens _lddvNextToken (\ s a -> s{_lddvNextToken = a})

-- | The maximum number of results to be returned per request.
lddvMaxResults :: Lens' ListDeviceDefinitionVersions (Maybe Text)
lddvMaxResults = lens _lddvMaxResults (\ s a -> s{_lddvMaxResults = a})

-- | The ID of the device definition.
lddvDeviceDefinitionId :: Lens' ListDeviceDefinitionVersions Text
lddvDeviceDefinitionId = lens _lddvDeviceDefinitionId (\ s a -> s{_lddvDeviceDefinitionId = a})

instance AWSRequest ListDeviceDefinitionVersions
         where
        type Rs ListDeviceDefinitionVersions =
             ListDeviceDefinitionVersionsResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 ListDeviceDefinitionVersionsResponse' <$>
                   (x .?> "Versions" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListDeviceDefinitionVersions where

instance NFData ListDeviceDefinitionVersions where

instance ToHeaders ListDeviceDefinitionVersions where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListDeviceDefinitionVersions where
        toPath ListDeviceDefinitionVersions'{..}
          = mconcat
              ["/greengrass/definition/devices/",
               toBS _lddvDeviceDefinitionId, "/versions"]

instance ToQuery ListDeviceDefinitionVersions where
        toQuery ListDeviceDefinitionVersions'{..}
          = mconcat
              ["NextToken" =: _lddvNextToken,
               "MaxResults" =: _lddvMaxResults]

-- | /See:/ 'listDeviceDefinitionVersionsResponse' smart constructor.
data ListDeviceDefinitionVersionsResponse = ListDeviceDefinitionVersionsResponse'
  { _lddvrsVersions       :: !(Maybe [VersionInformation])
  , _lddvrsNextToken      :: !(Maybe Text)
  , _lddvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeviceDefinitionVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lddvrsVersions' - Information about a version.
--
-- * 'lddvrsNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lddvrsResponseStatus' - -- | The response status code.
listDeviceDefinitionVersionsResponse
    :: Int -- ^ 'lddvrsResponseStatus'
    -> ListDeviceDefinitionVersionsResponse
listDeviceDefinitionVersionsResponse pResponseStatus_ =
  ListDeviceDefinitionVersionsResponse'
    { _lddvrsVersions = Nothing
    , _lddvrsNextToken = Nothing
    , _lddvrsResponseStatus = pResponseStatus_
    }


-- | Information about a version.
lddvrsVersions :: Lens' ListDeviceDefinitionVersionsResponse [VersionInformation]
lddvrsVersions = lens _lddvrsVersions (\ s a -> s{_lddvrsVersions = a}) . _Default . _Coerce

-- | The token for the next set of results, or ''null'' if there are no additional results.
lddvrsNextToken :: Lens' ListDeviceDefinitionVersionsResponse (Maybe Text)
lddvrsNextToken = lens _lddvrsNextToken (\ s a -> s{_lddvrsNextToken = a})

-- | -- | The response status code.
lddvrsResponseStatus :: Lens' ListDeviceDefinitionVersionsResponse Int
lddvrsResponseStatus = lens _lddvrsResponseStatus (\ s a -> s{_lddvrsResponseStatus = a})

instance NFData ListDeviceDefinitionVersionsResponse
         where

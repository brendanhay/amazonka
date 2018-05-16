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
-- Module      : Network.AWS.APIGateway.GetUsage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the usage data of a usage plan in a specified time interval.
--
--
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetUsage
    (
    -- * Creating a Request
      getUsage
    , GetUsage
    -- * Request Lenses
    , guKeyId
    , guLimit
    , guPosition
    , guUsagePlanId
    , guStartDate
    , guEndDate

    -- * Destructuring the Response
    , usage
    , Usage
    -- * Response Lenses
    , uUsagePlanId
    , uEndDate
    , uItems
    , uStartDate
    , uPosition
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The GET request to get the usage data of a usage plan in a specified time interval.
--
--
--
-- /See:/ 'getUsage' smart constructor.
data GetUsage = GetUsage'
  { _guKeyId       :: !(Maybe Text)
  , _guLimit       :: !(Maybe Int)
  , _guPosition    :: !(Maybe Text)
  , _guUsagePlanId :: !Text
  , _guStartDate   :: !Text
  , _guEndDate     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUsage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'guKeyId' - The Id of the API key associated with the resultant usage data.
--
-- * 'guLimit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- * 'guPosition' - The current pagination position in the paged result set.
--
-- * 'guUsagePlanId' - [Required] The Id of the usage plan associated with the usage data.
--
-- * 'guStartDate' - [Required] The starting date (e.g., 2016-01-01) of the usage data.
--
-- * 'guEndDate' - [Required] The ending date (e.g., 2016-12-31) of the usage data.
getUsage
    :: Text -- ^ 'guUsagePlanId'
    -> Text -- ^ 'guStartDate'
    -> Text -- ^ 'guEndDate'
    -> GetUsage
getUsage pUsagePlanId_ pStartDate_ pEndDate_ =
  GetUsage'
    { _guKeyId = Nothing
    , _guLimit = Nothing
    , _guPosition = Nothing
    , _guUsagePlanId = pUsagePlanId_
    , _guStartDate = pStartDate_
    , _guEndDate = pEndDate_
    }


-- | The Id of the API key associated with the resultant usage data.
guKeyId :: Lens' GetUsage (Maybe Text)
guKeyId = lens _guKeyId (\ s a -> s{_guKeyId = a})

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
guLimit :: Lens' GetUsage (Maybe Int)
guLimit = lens _guLimit (\ s a -> s{_guLimit = a})

-- | The current pagination position in the paged result set.
guPosition :: Lens' GetUsage (Maybe Text)
guPosition = lens _guPosition (\ s a -> s{_guPosition = a})

-- | [Required] The Id of the usage plan associated with the usage data.
guUsagePlanId :: Lens' GetUsage Text
guUsagePlanId = lens _guUsagePlanId (\ s a -> s{_guUsagePlanId = a})

-- | [Required] The starting date (e.g., 2016-01-01) of the usage data.
guStartDate :: Lens' GetUsage Text
guStartDate = lens _guStartDate (\ s a -> s{_guStartDate = a})

-- | [Required] The ending date (e.g., 2016-12-31) of the usage data.
guEndDate :: Lens' GetUsage Text
guEndDate = lens _guEndDate (\ s a -> s{_guEndDate = a})

instance AWSPager GetUsage where
        page rq rs
          | stop (rs ^. uPosition) = Nothing
          | stop (rs ^. uItems) = Nothing
          | otherwise =
            Just $ rq & guPosition .~ rs ^. uPosition

instance AWSRequest GetUsage where
        type Rs GetUsage = Usage
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetUsage where

instance NFData GetUsage where

instance ToHeaders GetUsage where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetUsage where
        toPath GetUsage'{..}
          = mconcat
              ["/usageplans/", toBS _guUsagePlanId, "/usage"]

instance ToQuery GetUsage where
        toQuery GetUsage'{..}
          = mconcat
              ["keyId" =: _guKeyId, "limit" =: _guLimit,
               "position" =: _guPosition,
               "startDate" =: _guStartDate, "endDate" =: _guEndDate]

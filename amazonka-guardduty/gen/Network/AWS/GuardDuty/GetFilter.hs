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
-- Module      : Network.AWS.GuardDuty.GetFilter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of the filter specified by the filter name.
module Network.AWS.GuardDuty.GetFilter
    (
    -- * Creating a Request
      getFilter
    , GetFilter
    -- * Request Lenses
    , gDetectorId
    , gFilterName

    -- * Destructuring the Response
    , getFilterResponse
    , GetFilterResponse
    -- * Response Lenses
    , gfrsFindingCriteria
    , gfrsAction
    , gfrsName
    , gfrsDescription
    , gfrsRank
    , gfrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getFilter' smart constructor.
data GetFilter = GetFilter'
  { _gDetectorId :: !Text
  , _gFilterName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gDetectorId' - The detector ID that specifies the GuardDuty service where you want to list the details of the specified filter.
--
-- * 'gFilterName' - The name of the filter whose details you want to get.
getFilter
    :: Text -- ^ 'gDetectorId'
    -> Text -- ^ 'gFilterName'
    -> GetFilter
getFilter pDetectorId_ pFilterName_ =
  GetFilter' {_gDetectorId = pDetectorId_, _gFilterName = pFilterName_}


-- | The detector ID that specifies the GuardDuty service where you want to list the details of the specified filter.
gDetectorId :: Lens' GetFilter Text
gDetectorId = lens _gDetectorId (\ s a -> s{_gDetectorId = a})

-- | The name of the filter whose details you want to get.
gFilterName :: Lens' GetFilter Text
gFilterName = lens _gFilterName (\ s a -> s{_gFilterName = a})

instance AWSRequest GetFilter where
        type Rs GetFilter = GetFilterResponse
        request = get guardDuty
        response
          = receiveJSON
              (\ s h x ->
                 GetFilterResponse' <$>
                   (x .?> "findingCriteria") <*> (x .?> "action") <*>
                     (x .?> "name")
                     <*> (x .?> "description")
                     <*> (x .?> "rank")
                     <*> (pure (fromEnum s)))

instance Hashable GetFilter where

instance NFData GetFilter where

instance ToHeaders GetFilter where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetFilter where
        toPath GetFilter'{..}
          = mconcat
              ["/detector/", toBS _gDetectorId, "/filter/",
               toBS _gFilterName]

instance ToQuery GetFilter where
        toQuery = const mempty

-- | /See:/ 'getFilterResponse' smart constructor.
data GetFilterResponse = GetFilterResponse'
  { _gfrsFindingCriteria :: !(Maybe FindingCriteria)
  , _gfrsAction          :: !(Maybe FilterAction)
  , _gfrsName            :: !(Maybe Text)
  , _gfrsDescription     :: !(Maybe Text)
  , _gfrsRank            :: !(Maybe Int)
  , _gfrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFilterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfrsFindingCriteria' - Represents the criteria to be used in the filter for querying findings.
--
-- * 'gfrsAction' - Specifies the action that is to be applied to the findings that match the filter.
--
-- * 'gfrsName' - The name of the filter.
--
-- * 'gfrsDescription' - The description of the filter.
--
-- * 'gfrsRank' - Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
--
-- * 'gfrsResponseStatus' - -- | The response status code.
getFilterResponse
    :: Int -- ^ 'gfrsResponseStatus'
    -> GetFilterResponse
getFilterResponse pResponseStatus_ =
  GetFilterResponse'
    { _gfrsFindingCriteria = Nothing
    , _gfrsAction = Nothing
    , _gfrsName = Nothing
    , _gfrsDescription = Nothing
    , _gfrsRank = Nothing
    , _gfrsResponseStatus = pResponseStatus_
    }


-- | Represents the criteria to be used in the filter for querying findings.
gfrsFindingCriteria :: Lens' GetFilterResponse (Maybe FindingCriteria)
gfrsFindingCriteria = lens _gfrsFindingCriteria (\ s a -> s{_gfrsFindingCriteria = a})

-- | Specifies the action that is to be applied to the findings that match the filter.
gfrsAction :: Lens' GetFilterResponse (Maybe FilterAction)
gfrsAction = lens _gfrsAction (\ s a -> s{_gfrsAction = a})

-- | The name of the filter.
gfrsName :: Lens' GetFilterResponse (Maybe Text)
gfrsName = lens _gfrsName (\ s a -> s{_gfrsName = a})

-- | The description of the filter.
gfrsDescription :: Lens' GetFilterResponse (Maybe Text)
gfrsDescription = lens _gfrsDescription (\ s a -> s{_gfrsDescription = a})

-- | Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
gfrsRank :: Lens' GetFilterResponse (Maybe Int)
gfrsRank = lens _gfrsRank (\ s a -> s{_gfrsRank = a})

-- | -- | The response status code.
gfrsResponseStatus :: Lens' GetFilterResponse Int
gfrsResponseStatus = lens _gfrsResponseStatus (\ s a -> s{_gfrsResponseStatus = a})

instance NFData GetFilterResponse where

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
-- Module      : Network.AWS.GuardDuty.CreateFilter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a filter using the specified finding criteria.
module Network.AWS.GuardDuty.CreateFilter
    (
    -- * Creating a Request
      createFilter
    , CreateFilter
    -- * Request Lenses
    , cfClientToken
    , cfAction
    , cfDescription
    , cfRank
    , cfDetectorId
    , cfFindingCriteria
    , cfName

    -- * Destructuring the Response
    , createFilterResponse
    , CreateFilterResponse
    -- * Response Lenses
    , cfrsName
    , cfrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | CreateFilterRequest request body.
--
-- /See:/ 'createFilter' smart constructor.
data CreateFilter = CreateFilter'
  { _cfClientToken     :: !(Maybe Text)
  , _cfAction          :: !(Maybe FilterAction)
  , _cfDescription     :: !(Maybe Text)
  , _cfRank            :: !(Maybe Int)
  , _cfDetectorId      :: !Text
  , _cfFindingCriteria :: !FindingCriteria
  , _cfName            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfClientToken' - The idempotency token for the create request.
--
-- * 'cfAction' - Specifies the action that is to be applied to the findings that match the filter.
--
-- * 'cfDescription' - The description of the filter.
--
-- * 'cfRank' - Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
--
-- * 'cfDetectorId' - The unique ID of the detector that you want to update.
--
-- * 'cfFindingCriteria' - Represents the criteria to be used in the filter for querying findings.
--
-- * 'cfName' - The name of the filter.
createFilter
    :: Text -- ^ 'cfDetectorId'
    -> FindingCriteria -- ^ 'cfFindingCriteria'
    -> Text -- ^ 'cfName'
    -> CreateFilter
createFilter pDetectorId_ pFindingCriteria_ pName_ =
  CreateFilter'
    { _cfClientToken = Nothing
    , _cfAction = Nothing
    , _cfDescription = Nothing
    , _cfRank = Nothing
    , _cfDetectorId = pDetectorId_
    , _cfFindingCriteria = pFindingCriteria_
    , _cfName = pName_
    }


-- | The idempotency token for the create request.
cfClientToken :: Lens' CreateFilter (Maybe Text)
cfClientToken = lens _cfClientToken (\ s a -> s{_cfClientToken = a})

-- | Specifies the action that is to be applied to the findings that match the filter.
cfAction :: Lens' CreateFilter (Maybe FilterAction)
cfAction = lens _cfAction (\ s a -> s{_cfAction = a})

-- | The description of the filter.
cfDescription :: Lens' CreateFilter (Maybe Text)
cfDescription = lens _cfDescription (\ s a -> s{_cfDescription = a})

-- | Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
cfRank :: Lens' CreateFilter (Maybe Int)
cfRank = lens _cfRank (\ s a -> s{_cfRank = a})

-- | The unique ID of the detector that you want to update.
cfDetectorId :: Lens' CreateFilter Text
cfDetectorId = lens _cfDetectorId (\ s a -> s{_cfDetectorId = a})

-- | Represents the criteria to be used in the filter for querying findings.
cfFindingCriteria :: Lens' CreateFilter FindingCriteria
cfFindingCriteria = lens _cfFindingCriteria (\ s a -> s{_cfFindingCriteria = a})

-- | The name of the filter.
cfName :: Lens' CreateFilter Text
cfName = lens _cfName (\ s a -> s{_cfName = a})

instance AWSRequest CreateFilter where
        type Rs CreateFilter = CreateFilterResponse
        request = postJSON guardDuty
        response
          = receiveJSON
              (\ s h x ->
                 CreateFilterResponse' <$>
                   (x .?> "name") <*> (pure (fromEnum s)))

instance Hashable CreateFilter where

instance NFData CreateFilter where

instance ToHeaders CreateFilter where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateFilter where
        toJSON CreateFilter'{..}
          = object
              (catMaybes
                 [("clientToken" .=) <$> _cfClientToken,
                  ("action" .=) <$> _cfAction,
                  ("description" .=) <$> _cfDescription,
                  ("rank" .=) <$> _cfRank,
                  Just ("findingCriteria" .= _cfFindingCriteria),
                  Just ("name" .= _cfName)])

instance ToPath CreateFilter where
        toPath CreateFilter'{..}
          = mconcat
              ["/detector/", toBS _cfDetectorId, "/filter"]

instance ToQuery CreateFilter where
        toQuery = const mempty

-- | /See:/ 'createFilterResponse' smart constructor.
data CreateFilterResponse = CreateFilterResponse'
  { _cfrsName           :: !(Maybe Text)
  , _cfrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFilterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfrsName' - The name of the successfully created filter.
--
-- * 'cfrsResponseStatus' - -- | The response status code.
createFilterResponse
    :: Int -- ^ 'cfrsResponseStatus'
    -> CreateFilterResponse
createFilterResponse pResponseStatus_ =
  CreateFilterResponse'
    {_cfrsName = Nothing, _cfrsResponseStatus = pResponseStatus_}


-- | The name of the successfully created filter.
cfrsName :: Lens' CreateFilterResponse (Maybe Text)
cfrsName = lens _cfrsName (\ s a -> s{_cfrsName = a})

-- | -- | The response status code.
cfrsResponseStatus :: Lens' CreateFilterResponse Int
cfrsResponseStatus = lens _cfrsResponseStatus (\ s a -> s{_cfrsResponseStatus = a})

instance NFData CreateFilterResponse where

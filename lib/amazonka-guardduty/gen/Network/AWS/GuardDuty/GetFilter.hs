{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of the filter specified by the filter name.
module Network.AWS.GuardDuty.GetFilter
  ( -- * Creating a Request
    getFilter,
    GetFilter,

    -- * Request Lenses
    gDetectorId,
    gFilterName,

    -- * Destructuring the Response
    getFilterResponse,
    GetFilterResponse,

    -- * Response Lenses
    gfrsDescription,
    gfrsRank,
    gfrsTags,
    gfrsResponseStatus,
    gfrsName,
    gfrsAction,
    gfrsFindingCriteria,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getFilter' smart constructor.
data GetFilter = GetFilter'
  { _gDetectorId :: !Text,
    _gFilterName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gDetectorId' - The unique ID of the detector that the filter is associated with.
--
-- * 'gFilterName' - The name of the filter you want to get.
getFilter ::
  -- | 'gDetectorId'
  Text ->
  -- | 'gFilterName'
  Text ->
  GetFilter
getFilter pDetectorId_ pFilterName_ =
  GetFilter'
    { _gDetectorId = pDetectorId_,
      _gFilterName = pFilterName_
    }

-- | The unique ID of the detector that the filter is associated with.
gDetectorId :: Lens' GetFilter Text
gDetectorId = lens _gDetectorId (\s a -> s {_gDetectorId = a})

-- | The name of the filter you want to get.
gFilterName :: Lens' GetFilter Text
gFilterName = lens _gFilterName (\s a -> s {_gFilterName = a})

instance AWSRequest GetFilter where
  type Rs GetFilter = GetFilterResponse
  request = get guardDuty
  response =
    receiveJSON
      ( \s h x ->
          GetFilterResponse'
            <$> (x .?> "description")
            <*> (x .?> "rank")
            <*> (x .?> "tags" .!@ mempty)
            <*> (pure (fromEnum s))
            <*> (x .:> "name")
            <*> (x .:> "action")
            <*> (x .:> "findingCriteria")
      )

instance Hashable GetFilter

instance NFData GetFilter

instance ToHeaders GetFilter where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetFilter where
  toPath GetFilter' {..} =
    mconcat
      ["/detector/", toBS _gDetectorId, "/filter/", toBS _gFilterName]

instance ToQuery GetFilter where
  toQuery = const mempty

-- | /See:/ 'getFilterResponse' smart constructor.
data GetFilterResponse = GetFilterResponse'
  { _gfrsDescription ::
      !(Maybe Text),
    _gfrsRank :: !(Maybe Nat),
    _gfrsTags :: !(Maybe (Map Text (Text))),
    _gfrsResponseStatus :: !Int,
    _gfrsName :: !Text,
    _gfrsAction :: !FilterAction,
    _gfrsFindingCriteria :: !FindingCriteria
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetFilterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfrsDescription' - The description of the filter.
--
-- * 'gfrsRank' - Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
--
-- * 'gfrsTags' - The tags of the filter resource.
--
-- * 'gfrsResponseStatus' - -- | The response status code.
--
-- * 'gfrsName' - The name of the filter.
--
-- * 'gfrsAction' - Specifies the action that is to be applied to the findings that match the filter.
--
-- * 'gfrsFindingCriteria' - Represents the criteria to be used in the filter for querying findings.
getFilterResponse ::
  -- | 'gfrsResponseStatus'
  Int ->
  -- | 'gfrsName'
  Text ->
  -- | 'gfrsAction'
  FilterAction ->
  -- | 'gfrsFindingCriteria'
  FindingCriteria ->
  GetFilterResponse
getFilterResponse
  pResponseStatus_
  pName_
  pAction_
  pFindingCriteria_ =
    GetFilterResponse'
      { _gfrsDescription = Nothing,
        _gfrsRank = Nothing,
        _gfrsTags = Nothing,
        _gfrsResponseStatus = pResponseStatus_,
        _gfrsName = pName_,
        _gfrsAction = pAction_,
        _gfrsFindingCriteria = pFindingCriteria_
      }

-- | The description of the filter.
gfrsDescription :: Lens' GetFilterResponse (Maybe Text)
gfrsDescription = lens _gfrsDescription (\s a -> s {_gfrsDescription = a})

-- | Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
gfrsRank :: Lens' GetFilterResponse (Maybe Natural)
gfrsRank = lens _gfrsRank (\s a -> s {_gfrsRank = a}) . mapping _Nat

-- | The tags of the filter resource.
gfrsTags :: Lens' GetFilterResponse (HashMap Text (Text))
gfrsTags = lens _gfrsTags (\s a -> s {_gfrsTags = a}) . _Default . _Map

-- | -- | The response status code.
gfrsResponseStatus :: Lens' GetFilterResponse Int
gfrsResponseStatus = lens _gfrsResponseStatus (\s a -> s {_gfrsResponseStatus = a})

-- | The name of the filter.
gfrsName :: Lens' GetFilterResponse Text
gfrsName = lens _gfrsName (\s a -> s {_gfrsName = a})

-- | Specifies the action that is to be applied to the findings that match the filter.
gfrsAction :: Lens' GetFilterResponse FilterAction
gfrsAction = lens _gfrsAction (\s a -> s {_gfrsAction = a})

-- | Represents the criteria to be used in the filter for querying findings.
gfrsFindingCriteria :: Lens' GetFilterResponse FindingCriteria
gfrsFindingCriteria = lens _gfrsFindingCriteria (\s a -> s {_gfrsFindingCriteria = a})

instance NFData GetFilterResponse

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
-- Module      : Network.AWS.GuardDuty.UpdateFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the filter specified by the filter name.
module Network.AWS.GuardDuty.UpdateFilter
  ( -- * Creating a Request
    updateFilter,
    UpdateFilter,

    -- * Request Lenses
    ufFindingCriteria,
    ufAction,
    ufDescription,
    ufRank,
    ufDetectorId,
    ufFilterName,

    -- * Destructuring the Response
    updateFilterResponse,
    UpdateFilterResponse,

    -- * Response Lenses
    ufrsResponseStatus,
    ufrsName,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateFilter' smart constructor.
data UpdateFilter = UpdateFilter'
  { _ufFindingCriteria ::
      !(Maybe FindingCriteria),
    _ufAction :: !(Maybe FilterAction),
    _ufDescription :: !(Maybe Text),
    _ufRank :: !(Maybe Nat),
    _ufDetectorId :: !Text,
    _ufFilterName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufFindingCriteria' - Represents the criteria to be used in the filter for querying findings.
--
-- * 'ufAction' - Specifies the action that is to be applied to the findings that match the filter.
--
-- * 'ufDescription' - The description of the filter.
--
-- * 'ufRank' - Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
--
-- * 'ufDetectorId' - The unique ID of the detector that specifies the GuardDuty service where you want to update a filter.
--
-- * 'ufFilterName' - The name of the filter.
updateFilter ::
  -- | 'ufDetectorId'
  Text ->
  -- | 'ufFilterName'
  Text ->
  UpdateFilter
updateFilter pDetectorId_ pFilterName_ =
  UpdateFilter'
    { _ufFindingCriteria = Nothing,
      _ufAction = Nothing,
      _ufDescription = Nothing,
      _ufRank = Nothing,
      _ufDetectorId = pDetectorId_,
      _ufFilterName = pFilterName_
    }

-- | Represents the criteria to be used in the filter for querying findings.
ufFindingCriteria :: Lens' UpdateFilter (Maybe FindingCriteria)
ufFindingCriteria = lens _ufFindingCriteria (\s a -> s {_ufFindingCriteria = a})

-- | Specifies the action that is to be applied to the findings that match the filter.
ufAction :: Lens' UpdateFilter (Maybe FilterAction)
ufAction = lens _ufAction (\s a -> s {_ufAction = a})

-- | The description of the filter.
ufDescription :: Lens' UpdateFilter (Maybe Text)
ufDescription = lens _ufDescription (\s a -> s {_ufDescription = a})

-- | Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
ufRank :: Lens' UpdateFilter (Maybe Natural)
ufRank = lens _ufRank (\s a -> s {_ufRank = a}) . mapping _Nat

-- | The unique ID of the detector that specifies the GuardDuty service where you want to update a filter.
ufDetectorId :: Lens' UpdateFilter Text
ufDetectorId = lens _ufDetectorId (\s a -> s {_ufDetectorId = a})

-- | The name of the filter.
ufFilterName :: Lens' UpdateFilter Text
ufFilterName = lens _ufFilterName (\s a -> s {_ufFilterName = a})

instance AWSRequest UpdateFilter where
  type Rs UpdateFilter = UpdateFilterResponse
  request = postJSON guardDuty
  response =
    receiveJSON
      ( \s h x ->
          UpdateFilterResponse' <$> (pure (fromEnum s)) <*> (x .:> "name")
      )

instance Hashable UpdateFilter

instance NFData UpdateFilter

instance ToHeaders UpdateFilter where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateFilter where
  toJSON UpdateFilter' {..} =
    object
      ( catMaybes
          [ ("findingCriteria" .=) <$> _ufFindingCriteria,
            ("action" .=) <$> _ufAction,
            ("description" .=) <$> _ufDescription,
            ("rank" .=) <$> _ufRank
          ]
      )

instance ToPath UpdateFilter where
  toPath UpdateFilter' {..} =
    mconcat
      ["/detector/", toBS _ufDetectorId, "/filter/", toBS _ufFilterName]

instance ToQuery UpdateFilter where
  toQuery = const mempty

-- | /See:/ 'updateFilterResponse' smart constructor.
data UpdateFilterResponse = UpdateFilterResponse'
  { _ufrsResponseStatus ::
      !Int,
    _ufrsName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateFilterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufrsResponseStatus' - -- | The response status code.
--
-- * 'ufrsName' - The name of the filter.
updateFilterResponse ::
  -- | 'ufrsResponseStatus'
  Int ->
  -- | 'ufrsName'
  Text ->
  UpdateFilterResponse
updateFilterResponse pResponseStatus_ pName_ =
  UpdateFilterResponse'
    { _ufrsResponseStatus = pResponseStatus_,
      _ufrsName = pName_
    }

-- | -- | The response status code.
ufrsResponseStatus :: Lens' UpdateFilterResponse Int
ufrsResponseStatus = lens _ufrsResponseStatus (\s a -> s {_ufrsResponseStatus = a})

-- | The name of the filter.
ufrsName :: Lens' UpdateFilterResponse Text
ufrsName = lens _ufrsName (\s a -> s {_ufrsName = a})

instance NFData UpdateFilterResponse

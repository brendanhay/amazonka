{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ImportJobsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ImportJobsResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.ImportJobResponse
import Network.AWS.Prelude

-- | Provides information about the status and settings of all the import jobs that are associated with an application or segment. An import job is a job that imports endpoint definitions from one or more files.
--
--
--
-- /See:/ 'importJobsResponse' smart constructor.
data ImportJobsResponse = ImportJobsResponse'
  { _ijNextToken ::
      !(Maybe Text),
    _ijItem :: ![ImportJobResponse]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImportJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ijNextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- * 'ijItem' - An array of responses, one for each import job that's associated with the application (Import Jobs resource) or segment (Segment Import Jobs resource).
importJobsResponse ::
  ImportJobsResponse
importJobsResponse =
  ImportJobsResponse' {_ijNextToken = Nothing, _ijItem = mempty}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
ijNextToken :: Lens' ImportJobsResponse (Maybe Text)
ijNextToken = lens _ijNextToken (\s a -> s {_ijNextToken = a})

-- | An array of responses, one for each import job that's associated with the application (Import Jobs resource) or segment (Segment Import Jobs resource).
ijItem :: Lens' ImportJobsResponse [ImportJobResponse]
ijItem = lens _ijItem (\s a -> s {_ijItem = a}) . _Coerce

instance FromJSON ImportJobsResponse where
  parseJSON =
    withObject
      "ImportJobsResponse"
      ( \x ->
          ImportJobsResponse'
            <$> (x .:? "NextToken") <*> (x .:? "Item" .!= mempty)
      )

instance Hashable ImportJobsResponse

instance NFData ImportJobsResponse

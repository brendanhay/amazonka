{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ExportJobsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ExportJobsResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.ExportJobResponse
import Network.AWS.Prelude

-- | Provides information about all the export jobs that are associated with an application or segment. An export job is a job that exports endpoint definitions to a file.
--
--
--
-- /See:/ 'exportJobsResponse' smart constructor.
data ExportJobsResponse = ExportJobsResponse'
  { _ejNextToken ::
      !(Maybe Text),
    _ejItem :: ![ExportJobResponse]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ejNextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- * 'ejItem' - An array of responses, one for each export job that's associated with the application (Export Jobs resource) or segment (Segment Export Jobs resource).
exportJobsResponse ::
  ExportJobsResponse
exportJobsResponse =
  ExportJobsResponse' {_ejNextToken = Nothing, _ejItem = mempty}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
ejNextToken :: Lens' ExportJobsResponse (Maybe Text)
ejNextToken = lens _ejNextToken (\s a -> s {_ejNextToken = a})

-- | An array of responses, one for each export job that's associated with the application (Export Jobs resource) or segment (Segment Export Jobs resource).
ejItem :: Lens' ExportJobsResponse [ExportJobResponse]
ejItem = lens _ejItem (\s a -> s {_ejItem = a}) . _Coerce

instance FromJSON ExportJobsResponse where
  parseJSON =
    withObject
      "ExportJobsResponse"
      ( \x ->
          ExportJobsResponse'
            <$> (x .:? "NextToken") <*> (x .:? "Item" .!= mempty)
      )

instance Hashable ExportJobsResponse

instance NFData ExportJobsResponse

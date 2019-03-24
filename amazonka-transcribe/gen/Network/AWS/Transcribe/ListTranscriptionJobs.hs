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
-- Module      : Network.AWS.Transcribe.ListTranscriptionJobs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists transcription jobs with the specified status.
--
--
module Network.AWS.Transcribe.ListTranscriptionJobs
    (
    -- * Creating a Request
      listTranscriptionJobs
    , ListTranscriptionJobs
    -- * Request Lenses
    , ltjStatus
    , ltjNextToken
    , ltjJobNameContains
    , ltjMaxResults

    -- * Destructuring the Response
    , listTranscriptionJobsResponse
    , ListTranscriptionJobsResponse
    -- * Response Lenses
    , ltjrsStatus
    , ltjrsNextToken
    , ltjrsTranscriptionJobSummaries
    , ltjrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types
import Network.AWS.Transcribe.Types.Product

-- | /See:/ 'listTranscriptionJobs' smart constructor.
data ListTranscriptionJobs = ListTranscriptionJobs'
  { _ltjStatus          :: !(Maybe TranscriptionJobStatus)
  , _ltjNextToken       :: !(Maybe Text)
  , _ltjJobNameContains :: !(Maybe Text)
  , _ltjMaxResults      :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTranscriptionJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltjStatus' - When specified, returns only transcription jobs with the specified status. Jobs are ordered by creation date, with the newest jobs returned first. If you don

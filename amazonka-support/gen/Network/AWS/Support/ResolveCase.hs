{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.ResolveCase
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Takes a CaseId and returns the initial state of the case along with the
-- state of the case after the call to ResolveCase completed.
module Network.AWS.Support
    (
    -- * Request
      ResolveCase
    -- ** Request constructor
    , mkResolveCase
    -- ** Request lenses
    , rcCaseId

    -- * Response
    , ResolveCaseResponse
    -- ** Response constructor
    , mkResolveCaseResponse
    -- ** Response lenses
    , rcrInitialCaseStatus
    , rcrFinalCaseStatus
    ) where

import Network.AWS.Support.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype ResolveCase = ResolveCase
    { _rcCaseId :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ResolveCase' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CaseId ::@ @Maybe Text@
--
mkResolveCase :: ResolveCase
mkResolveCase = ResolveCase
    { _rcCaseId = Nothing
    }

-- | The AWS Support case ID requested or returned in the call. The case ID is
-- an alphanumeric string formatted as shown in this example:
-- case-12345678910-2013-c4c1d2bf33c5cf47.
rcCaseId :: Lens' ResolveCase (Maybe Text)
rcCaseId = lens _rcCaseId (\s a -> s { _rcCaseId = a })

instance ToPath ResolveCase

instance ToQuery ResolveCase

instance ToHeaders ResolveCase

instance ToJSON ResolveCase

-- | The status of the case returned by the ResolveCase operation.
data ResolveCaseResponse = ResolveCaseResponse
    { _rcrInitialCaseStatus :: !(Maybe Text)
    , _rcrFinalCaseStatus :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ResolveCaseResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InitialCaseStatus ::@ @Maybe Text@
--
-- * @FinalCaseStatus ::@ @Maybe Text@
--
mkResolveCaseResponse :: ResolveCaseResponse
mkResolveCaseResponse = ResolveCaseResponse
    { _rcrInitialCaseStatus = Nothing
    , _rcrFinalCaseStatus = Nothing
    }

-- | The status of the case when the ResolveCase request was sent.
rcrInitialCaseStatus :: Lens' ResolveCaseResponse (Maybe Text)
rcrInitialCaseStatus =
    lens _rcrInitialCaseStatus (\s a -> s { _rcrInitialCaseStatus = a })

-- | The status of the case after the ResolveCase request was processed.
rcrFinalCaseStatus :: Lens' ResolveCaseResponse (Maybe Text)
rcrFinalCaseStatus =
    lens _rcrFinalCaseStatus (\s a -> s { _rcrFinalCaseStatus = a })

instance FromJSON ResolveCaseResponse

instance AWSRequest ResolveCase where
    type Sv ResolveCase = Support
    type Rs ResolveCase = ResolveCaseResponse

    request = get
    response _ = jsonResponse

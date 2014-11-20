{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
--
-- <http://docs.aws.amazon.com/awssupport/latest/APIReference/API_ResolveCase.html>
module Network.AWS.Support.ResolveCase
    (
    -- * Request
      ResolveCase
    -- ** Request constructor
    , resolveCase
    -- ** Request lenses
    , rcCaseId

    -- * Response
    , ResolveCaseResponse
    -- ** Response constructor
    , resolveCaseResponse
    -- ** Response lenses
    , rcrFinalCaseStatus
    , rcrInitialCaseStatus
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Support.Types
import qualified GHC.Exts

newtype ResolveCase = ResolveCase
    { _rcCaseId :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'ResolveCase' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcCaseId' @::@ 'Maybe' 'Text'
--
resolveCase :: ResolveCase
resolveCase = ResolveCase
    { _rcCaseId = Nothing
    }

-- | The AWS Support case ID requested or returned in the call. The case ID is
-- an alphanumeric string formatted as shown in this example:
-- case-12345678910-2013-c4c1d2bf33c5cf47.
rcCaseId :: Lens' ResolveCase (Maybe Text)
rcCaseId = lens _rcCaseId (\s a -> s { _rcCaseId = a })

data ResolveCaseResponse = ResolveCaseResponse
    { _rcrFinalCaseStatus   :: Maybe Text
    , _rcrInitialCaseStatus :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'ResolveCaseResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcrFinalCaseStatus' @::@ 'Maybe' 'Text'
--
-- * 'rcrInitialCaseStatus' @::@ 'Maybe' 'Text'
--
resolveCaseResponse :: ResolveCaseResponse
resolveCaseResponse = ResolveCaseResponse
    { _rcrInitialCaseStatus = Nothing
    , _rcrFinalCaseStatus   = Nothing
    }

-- | The status of the case after the ResolveCase request was processed.
rcrFinalCaseStatus :: Lens' ResolveCaseResponse (Maybe Text)
rcrFinalCaseStatus =
    lens _rcrFinalCaseStatus (\s a -> s { _rcrFinalCaseStatus = a })

-- | The status of the case when the ResolveCase request was sent.
rcrInitialCaseStatus :: Lens' ResolveCaseResponse (Maybe Text)
rcrInitialCaseStatus =
    lens _rcrInitialCaseStatus (\s a -> s { _rcrInitialCaseStatus = a })

instance ToPath ResolveCase where
    toPath = const "/"

instance ToQuery ResolveCase where
    toQuery = const mempty

instance ToHeaders ResolveCase

instance ToJSON ResolveCase where
    toJSON ResolveCase{..} = object
        [ "caseId" .= _rcCaseId
        ]

instance AWSRequest ResolveCase where
    type Sv ResolveCase = Support
    type Rs ResolveCase = ResolveCaseResponse

    request  = post "ResolveCase"
    response = jsonResponse

instance FromJSON ResolveCaseResponse where
    parseJSON = withObject "ResolveCaseResponse" $ \o -> ResolveCaseResponse
        <$> o .:? "finalCaseStatus"
        <*> o .:? "initialCaseStatus"

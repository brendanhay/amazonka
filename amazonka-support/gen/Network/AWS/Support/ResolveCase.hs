{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Support.ResolveCase
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Takes a @CaseId@ and returns the initial state of the case along with
-- the state of the case after the call to ResolveCase completed.
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
    , rcrInitialCaseStatus
    , rcrFinalCaseStatus
    , rcrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Support.Types

-- | /See:/ 'resolveCase' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcCaseId'
newtype ResolveCase = ResolveCase'
    { _rcCaseId :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'ResolveCase' smart constructor.
resolveCase :: ResolveCase
resolveCase =
    ResolveCase'
    { _rcCaseId = Nothing
    }

-- | The AWS Support case ID requested or returned in the call. The case ID
-- is an alphanumeric string formatted as shown in this example:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
rcCaseId :: Lens' ResolveCase (Maybe Text)
rcCaseId = lens _rcCaseId (\ s a -> s{_rcCaseId = a});

instance AWSRequest ResolveCase where
        type Sv ResolveCase = Support
        type Rs ResolveCase = ResolveCaseResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ResolveCaseResponse' <$>
                   (x .?> "initialCaseStatus") <*>
                     (x .?> "finalCaseStatus")
                     <*> (pure s))

instance ToHeaders ResolveCase where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSSupport_20130415.ResolveCase" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ResolveCase where
        toJSON ResolveCase'{..}
          = object ["caseId" .= _rcCaseId]

instance ToPath ResolveCase where
        toPath = const "/"

instance ToQuery ResolveCase where
        toQuery = const mempty

-- | The status of the case returned by the ResolveCase operation.
--
-- /See:/ 'resolveCaseResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcrInitialCaseStatus'
--
-- * 'rcrFinalCaseStatus'
--
-- * 'rcrStatus'
data ResolveCaseResponse = ResolveCaseResponse'
    { _rcrInitialCaseStatus :: !(Maybe Text)
    , _rcrFinalCaseStatus   :: !(Maybe Text)
    , _rcrStatus            :: !Status
    } deriving (Eq,Read,Show)

-- | 'ResolveCaseResponse' smart constructor.
resolveCaseResponse :: Status -> ResolveCaseResponse
resolveCaseResponse pStatus =
    ResolveCaseResponse'
    { _rcrInitialCaseStatus = Nothing
    , _rcrFinalCaseStatus = Nothing
    , _rcrStatus = pStatus
    }

-- | The status of the case when the ResolveCase request was sent.
rcrInitialCaseStatus :: Lens' ResolveCaseResponse (Maybe Text)
rcrInitialCaseStatus = lens _rcrInitialCaseStatus (\ s a -> s{_rcrInitialCaseStatus = a});

-- | The status of the case after the ResolveCase request was processed.
rcrFinalCaseStatus :: Lens' ResolveCaseResponse (Maybe Text)
rcrFinalCaseStatus = lens _rcrFinalCaseStatus (\ s a -> s{_rcrFinalCaseStatus = a});

-- | FIXME: Undocumented member.
rcrStatus :: Lens' ResolveCaseResponse Status
rcrStatus = lens _rcrStatus (\ s a -> s{_rcrStatus = a});

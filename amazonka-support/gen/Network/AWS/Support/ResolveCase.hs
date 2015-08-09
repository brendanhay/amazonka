{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.ResolveCase
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Takes a @CaseId@ and returns the initial state of the case along with
-- the state of the case after the call to ResolveCase completed.
--
-- /See:/ <http://docs.aws.amazon.com/awssupport/latest/APIReference/API_ResolveCase.html AWS API Reference> for ResolveCase.
module Network.AWS.Support.ResolveCase
    (
    -- * Creating a Request
      ResolveCase
    , resolveCase
    -- * Request Lenses
    , rcCaseId

    -- * Destructuring the Response
    , ResolveCaseResponse
    , resolveCaseResponse
    -- * Response Lenses
    , rcrsInitialCaseStatus
    , rcrsFinalCaseStatus
    , rcrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Support.Types
import           Network.AWS.Support.Types.Product

-- | /See:/ 'resolveCase' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcCaseId'
newtype ResolveCase = ResolveCase'
    { _rcCaseId :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
                     <*> (pure (fromEnum s)))

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
-- * 'rcrsInitialCaseStatus'
--
-- * 'rcrsFinalCaseStatus'
--
-- * 'rcrsStatus'
data ResolveCaseResponse = ResolveCaseResponse'
    { _rcrsInitialCaseStatus :: !(Maybe Text)
    , _rcrsFinalCaseStatus   :: !(Maybe Text)
    , _rcrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ResolveCaseResponse' smart constructor.
resolveCaseResponse :: Int -> ResolveCaseResponse
resolveCaseResponse pStatus_ =
    ResolveCaseResponse'
    { _rcrsInitialCaseStatus = Nothing
    , _rcrsFinalCaseStatus = Nothing
    , _rcrsStatus = pStatus_
    }

-- | The status of the case when the ResolveCase request was sent.
rcrsInitialCaseStatus :: Lens' ResolveCaseResponse (Maybe Text)
rcrsInitialCaseStatus = lens _rcrsInitialCaseStatus (\ s a -> s{_rcrsInitialCaseStatus = a});

-- | The status of the case after the ResolveCase request was processed.
rcrsFinalCaseStatus :: Lens' ResolveCaseResponse (Maybe Text)
rcrsFinalCaseStatus = lens _rcrsFinalCaseStatus (\ s a -> s{_rcrsFinalCaseStatus = a});

-- | Undocumented member.
rcrsStatus :: Lens' ResolveCaseResponse Int
rcrsStatus = lens _rcrsStatus (\ s a -> s{_rcrsStatus = a});

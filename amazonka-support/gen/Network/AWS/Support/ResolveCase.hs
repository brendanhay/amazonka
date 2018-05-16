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
-- Module      : Network.AWS.Support.ResolveCase
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Takes a @caseId@ and returns the initial state of the case along with the state of the case after the call to 'ResolveCase' completed.
--
--
module Network.AWS.Support.ResolveCase
    (
    -- * Creating a Request
      resolveCase
    , ResolveCase
    -- * Request Lenses
    , rcCaseId

    -- * Destructuring the Response
    , resolveCaseResponse
    , ResolveCaseResponse
    -- * Response Lenses
    , rcrsInitialCaseStatus
    , rcrsFinalCaseStatus
    , rcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Support.Types
import Network.AWS.Support.Types.Product

-- |
--
--
--
-- /See:/ 'resolveCase' smart constructor.
newtype ResolveCase = ResolveCase'
  { _rcCaseId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResolveCase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcCaseId' - The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
resolveCase
    :: ResolveCase
resolveCase = ResolveCase' {_rcCaseId = Nothing}


-- | The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
rcCaseId :: Lens' ResolveCase (Maybe Text)
rcCaseId = lens _rcCaseId (\ s a -> s{_rcCaseId = a})

instance AWSRequest ResolveCase where
        type Rs ResolveCase = ResolveCaseResponse
        request = postJSON support
        response
          = receiveJSON
              (\ s h x ->
                 ResolveCaseResponse' <$>
                   (x .?> "initialCaseStatus") <*>
                     (x .?> "finalCaseStatus")
                     <*> (pure (fromEnum s)))

instance Hashable ResolveCase where

instance NFData ResolveCase where

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
          = object (catMaybes [("caseId" .=) <$> _rcCaseId])

instance ToPath ResolveCase where
        toPath = const "/"

instance ToQuery ResolveCase where
        toQuery = const mempty

-- | The status of the case returned by the 'ResolveCase' operation.
--
--
--
-- /See:/ 'resolveCaseResponse' smart constructor.
data ResolveCaseResponse = ResolveCaseResponse'
  { _rcrsInitialCaseStatus :: !(Maybe Text)
  , _rcrsFinalCaseStatus   :: !(Maybe Text)
  , _rcrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResolveCaseResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcrsInitialCaseStatus' - The status of the case when the 'ResolveCase' request was sent.
--
-- * 'rcrsFinalCaseStatus' - The status of the case after the 'ResolveCase' request was processed.
--
-- * 'rcrsResponseStatus' - -- | The response status code.
resolveCaseResponse
    :: Int -- ^ 'rcrsResponseStatus'
    -> ResolveCaseResponse
resolveCaseResponse pResponseStatus_ =
  ResolveCaseResponse'
    { _rcrsInitialCaseStatus = Nothing
    , _rcrsFinalCaseStatus = Nothing
    , _rcrsResponseStatus = pResponseStatus_
    }


-- | The status of the case when the 'ResolveCase' request was sent.
rcrsInitialCaseStatus :: Lens' ResolveCaseResponse (Maybe Text)
rcrsInitialCaseStatus = lens _rcrsInitialCaseStatus (\ s a -> s{_rcrsInitialCaseStatus = a})

-- | The status of the case after the 'ResolveCase' request was processed.
rcrsFinalCaseStatus :: Lens' ResolveCaseResponse (Maybe Text)
rcrsFinalCaseStatus = lens _rcrsFinalCaseStatus (\ s a -> s{_rcrsFinalCaseStatus = a})

-- | -- | The response status code.
rcrsResponseStatus :: Lens' ResolveCaseResponse Int
rcrsResponseStatus = lens _rcrsResponseStatus (\ s a -> s{_rcrsResponseStatus = a})

instance NFData ResolveCaseResponse where

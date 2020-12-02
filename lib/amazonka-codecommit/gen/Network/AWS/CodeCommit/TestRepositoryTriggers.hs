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
-- Module      : Network.AWS.CodeCommit.TestRepositoryTriggers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests the functionality of repository triggers by sending information to the trigger target. If real data is available in the repository, the test will send data from the last commit. If no data is available, sample data will be generated.
--
--
module Network.AWS.CodeCommit.TestRepositoryTriggers
    (
    -- * Creating a Request
      testRepositoryTriggers
    , TestRepositoryTriggers
    -- * Request Lenses
    , trtRepositoryName
    , trtTriggers

    -- * Destructuring the Response
    , testRepositoryTriggersResponse
    , TestRepositoryTriggersResponse
    -- * Response Lenses
    , trtrsFailedExecutions
    , trtrsSuccessfulExecutions
    , trtrsResponseStatus
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a test repository triggers operation.
--
--
--
-- /See:/ 'testRepositoryTriggers' smart constructor.
data TestRepositoryTriggers = TestRepositoryTriggers'
  { _trtRepositoryName :: !Text
  , _trtTriggers       :: ![RepositoryTrigger]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TestRepositoryTriggers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trtRepositoryName' - The name of the repository in which to test the triggers.
--
-- * 'trtTriggers' - The list of triggers to test.
testRepositoryTriggers
    :: Text -- ^ 'trtRepositoryName'
    -> TestRepositoryTriggers
testRepositoryTriggers pRepositoryName_ =
  TestRepositoryTriggers'
    {_trtRepositoryName = pRepositoryName_, _trtTriggers = mempty}


-- | The name of the repository in which to test the triggers.
trtRepositoryName :: Lens' TestRepositoryTriggers Text
trtRepositoryName = lens _trtRepositoryName (\ s a -> s{_trtRepositoryName = a})

-- | The list of triggers to test.
trtTriggers :: Lens' TestRepositoryTriggers [RepositoryTrigger]
trtTriggers = lens _trtTriggers (\ s a -> s{_trtTriggers = a}) . _Coerce

instance AWSRequest TestRepositoryTriggers where
        type Rs TestRepositoryTriggers =
             TestRepositoryTriggersResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 TestRepositoryTriggersResponse' <$>
                   (x .?> "failedExecutions" .!@ mempty) <*>
                     (x .?> "successfulExecutions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable TestRepositoryTriggers where

instance NFData TestRepositoryTriggers where

instance ToHeaders TestRepositoryTriggers where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.TestRepositoryTriggers" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON TestRepositoryTriggers where
        toJSON TestRepositoryTriggers'{..}
          = object
              (catMaybes
                 [Just ("repositoryName" .= _trtRepositoryName),
                  Just ("triggers" .= _trtTriggers)])

instance ToPath TestRepositoryTriggers where
        toPath = const "/"

instance ToQuery TestRepositoryTriggers where
        toQuery = const mempty

-- | Represents the output of a test repository triggers operation.
--
--
--
-- /See:/ 'testRepositoryTriggersResponse' smart constructor.
data TestRepositoryTriggersResponse = TestRepositoryTriggersResponse'
  { _trtrsFailedExecutions     :: !(Maybe [RepositoryTriggerExecutionFailure])
  , _trtrsSuccessfulExecutions :: !(Maybe [Text])
  , _trtrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TestRepositoryTriggersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trtrsFailedExecutions' - The list of triggers that were not able to be tested. This list provides the names of the triggers that could not be tested, separated by commas.
--
-- * 'trtrsSuccessfulExecutions' - The list of triggers that were successfully tested. This list provides the names of the triggers that were successfully tested, separated by commas.
--
-- * 'trtrsResponseStatus' - -- | The response status code.
testRepositoryTriggersResponse
    :: Int -- ^ 'trtrsResponseStatus'
    -> TestRepositoryTriggersResponse
testRepositoryTriggersResponse pResponseStatus_ =
  TestRepositoryTriggersResponse'
    { _trtrsFailedExecutions = Nothing
    , _trtrsSuccessfulExecutions = Nothing
    , _trtrsResponseStatus = pResponseStatus_
    }


-- | The list of triggers that were not able to be tested. This list provides the names of the triggers that could not be tested, separated by commas.
trtrsFailedExecutions :: Lens' TestRepositoryTriggersResponse [RepositoryTriggerExecutionFailure]
trtrsFailedExecutions = lens _trtrsFailedExecutions (\ s a -> s{_trtrsFailedExecutions = a}) . _Default . _Coerce

-- | The list of triggers that were successfully tested. This list provides the names of the triggers that were successfully tested, separated by commas.
trtrsSuccessfulExecutions :: Lens' TestRepositoryTriggersResponse [Text]
trtrsSuccessfulExecutions = lens _trtrsSuccessfulExecutions (\ s a -> s{_trtrsSuccessfulExecutions = a}) . _Default . _Coerce

-- | -- | The response status code.
trtrsResponseStatus :: Lens' TestRepositoryTriggersResponse Int
trtrsResponseStatus = lens _trtrsResponseStatus (\ s a -> s{_trtrsResponseStatus = a})

instance NFData TestRepositoryTriggersResponse where

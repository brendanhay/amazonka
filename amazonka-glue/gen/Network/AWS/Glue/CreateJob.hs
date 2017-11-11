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
-- Module      : Network.AWS.Glue.CreateJob
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new job.
--
--
module Network.AWS.Glue.CreateJob
    (
    -- * Creating a Request
      createJob
    , CreateJob
    -- * Request Lenses
    , cjConnections
    , cjLogURI
    , cjMaxRetries
    , cjExecutionProperty
    , cjAllocatedCapacity
    , cjDefaultArguments
    , cjDescription
    , cjName
    , cjRole
    , cjCommand

    -- * Destructuring the Response
    , createJobResponse
    , CreateJobResponse
    -- * Response Lenses
    , cjrsName
    , cjrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createJob' smart constructor.
data CreateJob = CreateJob'
  { _cjConnections       :: {-# NOUNPACK #-}!(Maybe ConnectionsList)
  , _cjLogURI            :: {-# NOUNPACK #-}!(Maybe Text)
  , _cjMaxRetries        :: {-# NOUNPACK #-}!(Maybe Int)
  , _cjExecutionProperty :: {-# NOUNPACK #-}!(Maybe ExecutionProperty)
  , _cjAllocatedCapacity :: {-# NOUNPACK #-}!(Maybe Int)
  , _cjDefaultArguments  :: {-# NOUNPACK #-}!(Maybe (Map Text Text))
  , _cjDescription       :: {-# NOUNPACK #-}!(Maybe Text)
  , _cjName              :: {-# NOUNPACK #-}!Text
  , _cjRole              :: {-# NOUNPACK #-}!Text
  , _cjCommand           :: {-# NOUNPACK #-}!JobCommand
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjConnections' - The connections used for this job.
--
-- * 'cjLogURI' - Location of the logs for this job.
--
-- * 'cjMaxRetries' - The maximum number of times to retry this job if it fails.
--
-- * 'cjExecutionProperty' - An ExecutionProperty specifying the maximum number of concurrent runs allowed for this job.
--
-- * 'cjAllocatedCapacity' - The number of capacity units allocated to this job.
--
-- * 'cjDefaultArguments' - The default parameters for this job.
--
-- * 'cjDescription' - Description of the job.
--
-- * 'cjName' - The name you assign to this job.
--
-- * 'cjRole' - The role associated with this job.
--
-- * 'cjCommand' - The JobCommand that executes this job.
createJob
    :: Text -- ^ 'cjName'
    -> Text -- ^ 'cjRole'
    -> JobCommand -- ^ 'cjCommand'
    -> CreateJob
createJob pName_ pRole_ pCommand_ =
  CreateJob'
  { _cjConnections = Nothing
  , _cjLogURI = Nothing
  , _cjMaxRetries = Nothing
  , _cjExecutionProperty = Nothing
  , _cjAllocatedCapacity = Nothing
  , _cjDefaultArguments = Nothing
  , _cjDescription = Nothing
  , _cjName = pName_
  , _cjRole = pRole_
  , _cjCommand = pCommand_
  }


-- | The connections used for this job.
cjConnections :: Lens' CreateJob (Maybe ConnectionsList)
cjConnections = lens _cjConnections (\ s a -> s{_cjConnections = a});

-- | Location of the logs for this job.
cjLogURI :: Lens' CreateJob (Maybe Text)
cjLogURI = lens _cjLogURI (\ s a -> s{_cjLogURI = a});

-- | The maximum number of times to retry this job if it fails.
cjMaxRetries :: Lens' CreateJob (Maybe Int)
cjMaxRetries = lens _cjMaxRetries (\ s a -> s{_cjMaxRetries = a});

-- | An ExecutionProperty specifying the maximum number of concurrent runs allowed for this job.
cjExecutionProperty :: Lens' CreateJob (Maybe ExecutionProperty)
cjExecutionProperty = lens _cjExecutionProperty (\ s a -> s{_cjExecutionProperty = a});

-- | The number of capacity units allocated to this job.
cjAllocatedCapacity :: Lens' CreateJob (Maybe Int)
cjAllocatedCapacity = lens _cjAllocatedCapacity (\ s a -> s{_cjAllocatedCapacity = a});

-- | The default parameters for this job.
cjDefaultArguments :: Lens' CreateJob (HashMap Text Text)
cjDefaultArguments = lens _cjDefaultArguments (\ s a -> s{_cjDefaultArguments = a}) . _Default . _Map;

-- | Description of the job.
cjDescription :: Lens' CreateJob (Maybe Text)
cjDescription = lens _cjDescription (\ s a -> s{_cjDescription = a});

-- | The name you assign to this job.
cjName :: Lens' CreateJob Text
cjName = lens _cjName (\ s a -> s{_cjName = a});

-- | The role associated with this job.
cjRole :: Lens' CreateJob Text
cjRole = lens _cjRole (\ s a -> s{_cjRole = a});

-- | The JobCommand that executes this job.
cjCommand :: Lens' CreateJob JobCommand
cjCommand = lens _cjCommand (\ s a -> s{_cjCommand = a});

instance AWSRequest CreateJob where
        type Rs CreateJob = CreateJobResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 CreateJobResponse' <$>
                   (x .?> "Name") <*> (pure (fromEnum s)))

instance Hashable CreateJob where

instance NFData CreateJob where

instance ToHeaders CreateJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.CreateJob" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateJob where
        toJSON CreateJob'{..}
          = object
              (catMaybes
                 [("Connections" .=) <$> _cjConnections,
                  ("LogUri" .=) <$> _cjLogURI,
                  ("MaxRetries" .=) <$> _cjMaxRetries,
                  ("ExecutionProperty" .=) <$> _cjExecutionProperty,
                  ("AllocatedCapacity" .=) <$> _cjAllocatedCapacity,
                  ("DefaultArguments" .=) <$> _cjDefaultArguments,
                  ("Description" .=) <$> _cjDescription,
                  Just ("Name" .= _cjName), Just ("Role" .= _cjRole),
                  Just ("Command" .= _cjCommand)])

instance ToPath CreateJob where
        toPath = const "/"

instance ToQuery CreateJob where
        toQuery = const mempty

-- | /See:/ 'createJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { _cjrsName           :: {-# NOUNPACK #-}!(Maybe Text)
  , _cjrsResponseStatus :: {-# NOUNPACK #-}!Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjrsName' - The unique name of the new job that has been created.
--
-- * 'cjrsResponseStatus' - -- | The response status code.
createJobResponse
    :: Int -- ^ 'cjrsResponseStatus'
    -> CreateJobResponse
createJobResponse pResponseStatus_ =
  CreateJobResponse'
  {_cjrsName = Nothing, _cjrsResponseStatus = pResponseStatus_}


-- | The unique name of the new job that has been created.
cjrsName :: Lens' CreateJobResponse (Maybe Text)
cjrsName = lens _cjrsName (\ s a -> s{_cjrsName = a});

-- | -- | The response status code.
cjrsResponseStatus :: Lens' CreateJobResponse Int
cjrsResponseStatus = lens _cjrsResponseStatus (\ s a -> s{_cjrsResponseStatus = a});

instance NFData CreateJobResponse where

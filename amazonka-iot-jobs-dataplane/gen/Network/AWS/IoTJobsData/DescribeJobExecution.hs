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
-- Module      : Network.AWS.IoTJobsData.DescribeJobExecution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details of a job execution.
--
--
module Network.AWS.IoTJobsData.DescribeJobExecution
    (
    -- * Creating a Request
      describeJobExecution
    , DescribeJobExecution
    -- * Request Lenses
    , djeIncludeJobDocument
    , djeExecutionNumber
    , djeJobId
    , djeThingName

    -- * Destructuring the Response
    , describeJobExecutionResponse
    , DescribeJobExecutionResponse
    -- * Response Lenses
    , djersExecution
    , djersResponseStatus
    ) where

import Network.AWS.IoTJobsData.Types
import Network.AWS.IoTJobsData.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeJobExecution' smart constructor.
data DescribeJobExecution = DescribeJobExecution'
  { _djeIncludeJobDocument :: !(Maybe Bool)
  , _djeExecutionNumber    :: !(Maybe Integer)
  , _djeJobId              :: !Text
  , _djeThingName          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeJobExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djeIncludeJobDocument' - Optional. When set to true, the response contains the job document. The default is false.
--
-- * 'djeExecutionNumber' - Optional. A number that identifies a particular job execution on a particular device. If not specified, the latest job execution is returned.
--
-- * 'djeJobId' - The unique identifier assigned to this job when it was created.
--
-- * 'djeThingName' - The thing name associated with the device the job execution is running on.
describeJobExecution
    :: Text -- ^ 'djeJobId'
    -> Text -- ^ 'djeThingName'
    -> DescribeJobExecution
describeJobExecution pJobId_ pThingName_ =
  DescribeJobExecution'
    { _djeIncludeJobDocument = Nothing
    , _djeExecutionNumber = Nothing
    , _djeJobId = pJobId_
    , _djeThingName = pThingName_
    }


-- | Optional. When set to true, the response contains the job document. The default is false.
djeIncludeJobDocument :: Lens' DescribeJobExecution (Maybe Bool)
djeIncludeJobDocument = lens _djeIncludeJobDocument (\ s a -> s{_djeIncludeJobDocument = a})

-- | Optional. A number that identifies a particular job execution on a particular device. If not specified, the latest job execution is returned.
djeExecutionNumber :: Lens' DescribeJobExecution (Maybe Integer)
djeExecutionNumber = lens _djeExecutionNumber (\ s a -> s{_djeExecutionNumber = a})

-- | The unique identifier assigned to this job when it was created.
djeJobId :: Lens' DescribeJobExecution Text
djeJobId = lens _djeJobId (\ s a -> s{_djeJobId = a})

-- | The thing name associated with the device the job execution is running on.
djeThingName :: Lens' DescribeJobExecution Text
djeThingName = lens _djeThingName (\ s a -> s{_djeThingName = a})

instance AWSRequest DescribeJobExecution where
        type Rs DescribeJobExecution =
             DescribeJobExecutionResponse
        request = get ioTJobsData
        response
          = receiveJSON
              (\ s h x ->
                 DescribeJobExecutionResponse' <$>
                   (x .?> "execution") <*> (pure (fromEnum s)))

instance Hashable DescribeJobExecution where

instance NFData DescribeJobExecution where

instance ToHeaders DescribeJobExecution where
        toHeaders = const mempty

instance ToPath DescribeJobExecution where
        toPath DescribeJobExecution'{..}
          = mconcat
              ["/things/", toBS _djeThingName, "/jobs/",
               toBS _djeJobId]

instance ToQuery DescribeJobExecution where
        toQuery DescribeJobExecution'{..}
          = mconcat
              ["includeJobDocument" =: _djeIncludeJobDocument,
               "executionNumber" =: _djeExecutionNumber]

-- | /See:/ 'describeJobExecutionResponse' smart constructor.
data DescribeJobExecutionResponse = DescribeJobExecutionResponse'
  { _djersExecution      :: !(Maybe JobExecution)
  , _djersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeJobExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djersExecution' - Contains data about a job execution.
--
-- * 'djersResponseStatus' - -- | The response status code.
describeJobExecutionResponse
    :: Int -- ^ 'djersResponseStatus'
    -> DescribeJobExecutionResponse
describeJobExecutionResponse pResponseStatus_ =
  DescribeJobExecutionResponse'
    {_djersExecution = Nothing, _djersResponseStatus = pResponseStatus_}


-- | Contains data about a job execution.
djersExecution :: Lens' DescribeJobExecutionResponse (Maybe JobExecution)
djersExecution = lens _djersExecution (\ s a -> s{_djersExecution = a})

-- | -- | The response status code.
djersResponseStatus :: Lens' DescribeJobExecutionResponse Int
djersResponseStatus = lens _djersResponseStatus (\ s a -> s{_djersResponseStatus = a})

instance NFData DescribeJobExecutionResponse where

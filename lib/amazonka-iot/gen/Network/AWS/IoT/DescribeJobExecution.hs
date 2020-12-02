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
-- Module      : Network.AWS.IoT.DescribeJobExecution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a job execution.
--
--
module Network.AWS.IoT.DescribeJobExecution
    (
    -- * Creating a Request
      describeJobExecution
    , DescribeJobExecution
    -- * Request Lenses
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

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeJobExecution' smart constructor.
data DescribeJobExecution = DescribeJobExecution'
  { _djeExecutionNumber :: !(Maybe Integer)
  , _djeJobId           :: !Text
  , _djeThingName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeJobExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djeExecutionNumber' - A string (consisting of the digits "0" through "9" which is used to specify a particular job execution on a particular device.
--
-- * 'djeJobId' - The unique identifier you assigned to this job when it was created.
--
-- * 'djeThingName' - The name of the thing on which the job execution is running.
describeJobExecution
    :: Text -- ^ 'djeJobId'
    -> Text -- ^ 'djeThingName'
    -> DescribeJobExecution
describeJobExecution pJobId_ pThingName_ =
  DescribeJobExecution'
    { _djeExecutionNumber = Nothing
    , _djeJobId = pJobId_
    , _djeThingName = pThingName_
    }


-- | A string (consisting of the digits "0" through "9" which is used to specify a particular job execution on a particular device.
djeExecutionNumber :: Lens' DescribeJobExecution (Maybe Integer)
djeExecutionNumber = lens _djeExecutionNumber (\ s a -> s{_djeExecutionNumber = a})

-- | The unique identifier you assigned to this job when it was created.
djeJobId :: Lens' DescribeJobExecution Text
djeJobId = lens _djeJobId (\ s a -> s{_djeJobId = a})

-- | The name of the thing on which the job execution is running.
djeThingName :: Lens' DescribeJobExecution Text
djeThingName = lens _djeThingName (\ s a -> s{_djeThingName = a})

instance AWSRequest DescribeJobExecution where
        type Rs DescribeJobExecution =
             DescribeJobExecutionResponse
        request = get ioT
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
          = mconcat ["executionNumber" =: _djeExecutionNumber]

-- | /See:/ 'describeJobExecutionResponse' smart constructor.
data DescribeJobExecutionResponse = DescribeJobExecutionResponse'
  { _djersExecution      :: !(Maybe JobExecution)
  , _djersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeJobExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djersExecution' - Information about the job execution.
--
-- * 'djersResponseStatus' - -- | The response status code.
describeJobExecutionResponse
    :: Int -- ^ 'djersResponseStatus'
    -> DescribeJobExecutionResponse
describeJobExecutionResponse pResponseStatus_ =
  DescribeJobExecutionResponse'
    {_djersExecution = Nothing, _djersResponseStatus = pResponseStatus_}


-- | Information about the job execution.
djersExecution :: Lens' DescribeJobExecutionResponse (Maybe JobExecution)
djersExecution = lens _djersExecution (\ s a -> s{_djersExecution = a})

-- | -- | The response status code.
djersResponseStatus :: Lens' DescribeJobExecutionResponse Int
djersResponseStatus = lens _djersResponseStatus (\ s a -> s{_djersResponseStatus = a})

instance NFData DescribeJobExecutionResponse where

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
-- Module      : Network.AWS.OpsWorks.DescribeAgentVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available AWS OpsWorks Stacks agent versions. You must specify a stack ID or a configuration manager. @DescribeAgentVersions@ returns a list of available agent versions for the specified stack or configuration manager.
--
--
module Network.AWS.OpsWorks.DescribeAgentVersions
    (
    -- * Creating a Request
      describeAgentVersions
    , DescribeAgentVersions
    -- * Request Lenses
    , davConfigurationManager
    , davStackId

    -- * Destructuring the Response
    , describeAgentVersionsResponse
    , DescribeAgentVersionsResponse
    -- * Response Lenses
    , davrsAgentVersions
    , davrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAgentVersions' smart constructor.
data DescribeAgentVersions = DescribeAgentVersions'
  { _davConfigurationManager :: !(Maybe StackConfigurationManager)
  , _davStackId              :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAgentVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'davConfigurationManager' - The configuration manager.
--
-- * 'davStackId' - The stack ID.
describeAgentVersions
    :: DescribeAgentVersions
describeAgentVersions =
  DescribeAgentVersions'
    {_davConfigurationManager = Nothing, _davStackId = Nothing}


-- | The configuration manager.
davConfigurationManager :: Lens' DescribeAgentVersions (Maybe StackConfigurationManager)
davConfigurationManager = lens _davConfigurationManager (\ s a -> s{_davConfigurationManager = a})

-- | The stack ID.
davStackId :: Lens' DescribeAgentVersions (Maybe Text)
davStackId = lens _davStackId (\ s a -> s{_davStackId = a})

instance AWSRequest DescribeAgentVersions where
        type Rs DescribeAgentVersions =
             DescribeAgentVersionsResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAgentVersionsResponse' <$>
                   (x .?> "AgentVersions" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeAgentVersions where

instance NFData DescribeAgentVersions where

instance ToHeaders DescribeAgentVersions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeAgentVersions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeAgentVersions where
        toJSON DescribeAgentVersions'{..}
          = object
              (catMaybes
                 [("ConfigurationManager" .=) <$>
                    _davConfigurationManager,
                  ("StackId" .=) <$> _davStackId])

instance ToPath DescribeAgentVersions where
        toPath = const "/"

instance ToQuery DescribeAgentVersions where
        toQuery = const mempty

-- | Contains the response to a @DescribeAgentVersions@ request.
--
--
--
-- /See:/ 'describeAgentVersionsResponse' smart constructor.
data DescribeAgentVersionsResponse = DescribeAgentVersionsResponse'
  { _davrsAgentVersions  :: !(Maybe [AgentVersion])
  , _davrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAgentVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'davrsAgentVersions' - The agent versions for the specified stack or configuration manager. Note that this value is the complete version number, not the abbreviated number used by the console.
--
-- * 'davrsResponseStatus' - -- | The response status code.
describeAgentVersionsResponse
    :: Int -- ^ 'davrsResponseStatus'
    -> DescribeAgentVersionsResponse
describeAgentVersionsResponse pResponseStatus_ =
  DescribeAgentVersionsResponse'
    {_davrsAgentVersions = Nothing, _davrsResponseStatus = pResponseStatus_}


-- | The agent versions for the specified stack or configuration manager. Note that this value is the complete version number, not the abbreviated number used by the console.
davrsAgentVersions :: Lens' DescribeAgentVersionsResponse [AgentVersion]
davrsAgentVersions = lens _davrsAgentVersions (\ s a -> s{_davrsAgentVersions = a}) . _Default . _Coerce

-- | -- | The response status code.
davrsResponseStatus :: Lens' DescribeAgentVersionsResponse Int
davrsResponseStatus = lens _davrsResponseStatus (\ s a -> s{_davrsResponseStatus = a})

instance NFData DescribeAgentVersionsResponse where

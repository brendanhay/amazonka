{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.OpsWorks.DescribeAgentVersions
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

-- | Describes the available AWS OpsWorks agent versions. You must specify a
-- stack ID or a configuration manager. @DescribeAgentVersions@ returns a
-- list of available agent versions for the specified stack or
-- configuration manager.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeAgentVersions.html>
module Network.AWS.OpsWorks.DescribeAgentVersions
    (
    -- * Request
      DescribeAgentVersions
    -- ** Request constructor
    , describeAgentVersions
    -- ** Request lenses
    , davConfigurationManager
    , davStackId

    -- * Response
    , DescribeAgentVersionsResponse
    -- ** Response constructor
    , describeAgentVersionsResponse
    -- ** Response lenses
    , davrAgentVersions
    , davrStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeAgentVersions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'davConfigurationManager'
--
-- * 'davStackId'
data DescribeAgentVersions = DescribeAgentVersions'
    { _davConfigurationManager :: !(Maybe StackConfigurationManager)
    , _davStackId              :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'DescribeAgentVersions' smart constructor.
describeAgentVersions :: DescribeAgentVersions
describeAgentVersions =
    DescribeAgentVersions'
    { _davConfigurationManager = Nothing
    , _davStackId = Nothing
    }

-- | The configuration manager.
davConfigurationManager :: Lens' DescribeAgentVersions (Maybe StackConfigurationManager)
davConfigurationManager = lens _davConfigurationManager (\ s a -> s{_davConfigurationManager = a});

-- | The stack ID.
davStackId :: Lens' DescribeAgentVersions (Maybe Text)
davStackId = lens _davStackId (\ s a -> s{_davStackId = a});

instance AWSRequest DescribeAgentVersions where
        type Sv DescribeAgentVersions = OpsWorks
        type Rs DescribeAgentVersions =
             DescribeAgentVersionsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAgentVersionsResponse' <$>
                   (x .?> "AgentVersions" .!@ mempty) <*> (pure s))

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
              ["ConfigurationManager" .= _davConfigurationManager,
               "StackId" .= _davStackId]

instance ToPath DescribeAgentVersions where
        toPath = const "/"

instance ToQuery DescribeAgentVersions where
        toQuery = const mempty

-- | Contains the response to a @DescribeAgentVersions@ request.
--
-- /See:/ 'describeAgentVersionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'davrAgentVersions'
--
-- * 'davrStatus'
data DescribeAgentVersionsResponse = DescribeAgentVersionsResponse'
    { _davrAgentVersions :: !(Maybe [AgentVersion])
    , _davrStatus        :: !Status
    } deriving (Eq,Read,Show)

-- | 'DescribeAgentVersionsResponse' smart constructor.
describeAgentVersionsResponse :: Status -> DescribeAgentVersionsResponse
describeAgentVersionsResponse pStatus =
    DescribeAgentVersionsResponse'
    { _davrAgentVersions = Nothing
    , _davrStatus = pStatus
    }

-- | The agent versions for the specified stack or configuration manager.
-- Note that this value is the complete version number, not the abbreviated
-- number used by the console.
davrAgentVersions :: Lens' DescribeAgentVersionsResponse [AgentVersion]
davrAgentVersions = lens _davrAgentVersions (\ s a -> s{_davrAgentVersions = a}) . _Default;

-- | FIXME: Undocumented member.
davrStatus :: Lens' DescribeAgentVersionsResponse Status
davrStatus = lens _davrStatus (\ s a -> s{_davrStatus = a});

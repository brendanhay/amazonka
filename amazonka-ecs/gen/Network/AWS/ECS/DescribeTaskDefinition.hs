{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ECS.DescribeTaskDefinition
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

-- | Describes a task definition. You can specify a @family@ and @revision@
-- to find information on a specific task definition, or you can simply
-- specify the family to find the latest @ACTIVE@ revision in that family.
--
-- You can only describe @INACTIVE@ task definitions while an active task
-- or service references them.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeTaskDefinition.html>
module Network.AWS.ECS.DescribeTaskDefinition
    (
    -- * Request
      DescribeTaskDefinition
    -- ** Request constructor
    , describeTaskDefinition
    -- ** Request lenses
    , dtdTaskDefinition

    -- * Response
    , DescribeTaskDefinitionResponse
    -- ** Response constructor
    , describeTaskDefinitionResponse
    -- ** Response lenses
    , desTaskDefinition
    ) where

import Network.AWS.ECS.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeTaskDefinition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtdTaskDefinition'
newtype DescribeTaskDefinition = DescribeTaskDefinition'{_dtdTaskDefinition :: Text} deriving (Eq, Read, Show)

-- | 'DescribeTaskDefinition' smart constructor.
describeTaskDefinition :: Text -> DescribeTaskDefinition
describeTaskDefinition pTaskDefinition = DescribeTaskDefinition'{_dtdTaskDefinition = pTaskDefinition};

-- | The @family@ for the latest @ACTIVE@ revision, @family@ and @revision@
-- (@family:revision@) for a specific revision in the family, or full
-- Amazon Resource Name (ARN) of the task definition that you want to
-- describe.
dtdTaskDefinition :: Lens' DescribeTaskDefinition Text
dtdTaskDefinition = lens _dtdTaskDefinition (\ s a -> s{_dtdTaskDefinition = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DescribeTaskDefinition where
        type Sv DescribeTaskDefinition = ECS
        type Rs DescribeTaskDefinition =
             DescribeTaskDefinitionResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTaskDefinitionResponse' <$>
                   (x .?> "taskDefinition"))

instance ToHeaders DescribeTaskDefinition where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.DescribeTaskDefinition"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeTaskDefinition where
        toJSON DescribeTaskDefinition'{..}
          = object ["taskDefinition" .= _dtdTaskDefinition]

instance ToPath DescribeTaskDefinition where
        toPath = const "/"

instance ToQuery DescribeTaskDefinition where
        toQuery = const mempty

-- | /See:/ 'describeTaskDefinitionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desTaskDefinition'
newtype DescribeTaskDefinitionResponse = DescribeTaskDefinitionResponse'{_desTaskDefinition :: Maybe TaskDefinition} deriving (Eq, Read, Show)

-- | 'DescribeTaskDefinitionResponse' smart constructor.
describeTaskDefinitionResponse :: DescribeTaskDefinitionResponse
describeTaskDefinitionResponse = DescribeTaskDefinitionResponse'{_desTaskDefinition = Nothing};

-- | The full task definition description.
desTaskDefinition :: Lens' DescribeTaskDefinitionResponse (Maybe TaskDefinition)
desTaskDefinition = lens _desTaskDefinition (\ s a -> s{_desTaskDefinition = a});

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DescribeTaskDefinition
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes a task definition. You can specify a @family@ and @revision@
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
    , dtdrqTaskDefinition

    -- * Response
    , DescribeTaskDefinitionResponse
    -- ** Response constructor
    , describeTaskDefinitionResponse
    -- ** Response lenses
    , dtdrsTaskDefinition
    , dtdrsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeTaskDefinition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtdrqTaskDefinition'
newtype DescribeTaskDefinition = DescribeTaskDefinition'
    { _dtdrqTaskDefinition :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTaskDefinition' smart constructor.
describeTaskDefinition :: Text -> DescribeTaskDefinition
describeTaskDefinition pTaskDefinition =
    DescribeTaskDefinition'
    { _dtdrqTaskDefinition = pTaskDefinition
    }

-- | The @family@ for the latest @ACTIVE@ revision, @family@ and @revision@
-- (@family:revision@) for a specific revision in the family, or full
-- Amazon Resource Name (ARN) of the task definition that you want to
-- describe.
dtdrqTaskDefinition :: Lens' DescribeTaskDefinition Text
dtdrqTaskDefinition = lens _dtdrqTaskDefinition (\ s a -> s{_dtdrqTaskDefinition = a});

instance AWSRequest DescribeTaskDefinition where
        type Sv DescribeTaskDefinition = ECS
        type Rs DescribeTaskDefinition =
             DescribeTaskDefinitionResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTaskDefinitionResponse' <$>
                   (x .?> "taskDefinition") <*> (pure (fromEnum s)))

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
          = object ["taskDefinition" .= _dtdrqTaskDefinition]

instance ToPath DescribeTaskDefinition where
        toPath = const "/"

instance ToQuery DescribeTaskDefinition where
        toQuery = const mempty

-- | /See:/ 'describeTaskDefinitionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtdrsTaskDefinition'
--
-- * 'dtdrsStatus'
data DescribeTaskDefinitionResponse = DescribeTaskDefinitionResponse'
    { _dtdrsTaskDefinition :: !(Maybe TaskDefinition)
    , _dtdrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTaskDefinitionResponse' smart constructor.
describeTaskDefinitionResponse :: Int -> DescribeTaskDefinitionResponse
describeTaskDefinitionResponse pStatus =
    DescribeTaskDefinitionResponse'
    { _dtdrsTaskDefinition = Nothing
    , _dtdrsStatus = pStatus
    }

-- | The full task definition description.
dtdrsTaskDefinition :: Lens' DescribeTaskDefinitionResponse (Maybe TaskDefinition)
dtdrsTaskDefinition = lens _dtdrsTaskDefinition (\ s a -> s{_dtdrsTaskDefinition = a});

-- | FIXME: Undocumented member.
dtdrsStatus :: Lens' DescribeTaskDefinitionResponse Int
dtdrsStatus = lens _dtdrsStatus (\ s a -> s{_dtdrsStatus = a});

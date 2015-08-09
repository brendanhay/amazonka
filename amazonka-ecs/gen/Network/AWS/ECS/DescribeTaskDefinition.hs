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
-- Module      : Network.AWS.ECS.DescribeTaskDefinition
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a task definition. You can specify a @family@ and @revision@
-- to find information on a specific task definition, or you can simply
-- specify the family to find the latest @ACTIVE@ revision in that family.
--
-- You can only describe @INACTIVE@ task definitions while an active task
-- or service references them.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeTaskDefinition.html AWS API Reference> for DescribeTaskDefinition.
module Network.AWS.ECS.DescribeTaskDefinition
    (
    -- * Creating a Request
      DescribeTaskDefinition
    , describeTaskDefinition
    -- * Request Lenses
    , dtdTaskDefinition

    -- * Destructuring the Response
    , DescribeTaskDefinitionResponse
    , describeTaskDefinitionResponse
    -- * Response Lenses
    , desrsTaskDefinition
    , desrsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.ECS.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeTaskDefinition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtdTaskDefinition'
newtype DescribeTaskDefinition = DescribeTaskDefinition'
    { _dtdTaskDefinition :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTaskDefinition' smart constructor.
describeTaskDefinition :: Text -> DescribeTaskDefinition
describeTaskDefinition pTaskDefinition_ =
    DescribeTaskDefinition'
    { _dtdTaskDefinition = pTaskDefinition_
    }

-- | The @family@ for the latest @ACTIVE@ revision, @family@ and @revision@
-- (@family:revision@) for a specific revision in the family, or full
-- Amazon Resource Name (ARN) of the task definition that you want to
-- describe.
dtdTaskDefinition :: Lens' DescribeTaskDefinition Text
dtdTaskDefinition = lens _dtdTaskDefinition (\ s a -> s{_dtdTaskDefinition = a});

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
          = object ["taskDefinition" .= _dtdTaskDefinition]

instance ToPath DescribeTaskDefinition where
        toPath = const "/"

instance ToQuery DescribeTaskDefinition where
        toQuery = const mempty

-- | /See:/ 'describeTaskDefinitionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desrsTaskDefinition'
--
-- * 'desrsStatus'
data DescribeTaskDefinitionResponse = DescribeTaskDefinitionResponse'
    { _desrsTaskDefinition :: !(Maybe TaskDefinition)
    , _desrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTaskDefinitionResponse' smart constructor.
describeTaskDefinitionResponse :: Int -> DescribeTaskDefinitionResponse
describeTaskDefinitionResponse pStatus_ =
    DescribeTaskDefinitionResponse'
    { _desrsTaskDefinition = Nothing
    , _desrsStatus = pStatus_
    }

-- | The full task definition description.
desrsTaskDefinition :: Lens' DescribeTaskDefinitionResponse (Maybe TaskDefinition)
desrsTaskDefinition = lens _desrsTaskDefinition (\ s a -> s{_desrsTaskDefinition = a});

-- | Undocumented member.
desrsStatus :: Lens' DescribeTaskDefinitionResponse Int
desrsStatus = lens _desrsStatus (\ s a -> s{_desrsStatus = a});

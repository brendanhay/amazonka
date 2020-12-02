{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DescribeTaskSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the task sets in the specified cluster and service. This is used when a service uses the @EXTERNAL@ deployment controller type. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.DescribeTaskSets
  ( -- * Creating a Request
    describeTaskSets,
    DescribeTaskSets,

    -- * Request Lenses
    dtssTaskSets,
    dtssInclude,
    dtssCluster,
    dtssService,

    -- * Destructuring the Response
    describeTaskSetsResponse,
    DescribeTaskSetsResponse,

    -- * Response Lenses
    dtssrsTaskSets,
    dtssrsFailures,
    dtssrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeTaskSets' smart constructor.
data DescribeTaskSets = DescribeTaskSets'
  { _dtssTaskSets ::
      !(Maybe [Text]),
    _dtssInclude :: !(Maybe [TaskSetField]),
    _dtssCluster :: !Text,
    _dtssService :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTaskSets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtssTaskSets' - The ID or full Amazon Resource Name (ARN) of task sets to describe.
--
-- * 'dtssInclude' - Specifies whether to see the resource tags for the task set. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
--
-- * 'dtssCluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task sets exist in.
--
-- * 'dtssService' - The short name or full Amazon Resource Name (ARN) of the service that the task sets exist in.
describeTaskSets ::
  -- | 'dtssCluster'
  Text ->
  -- | 'dtssService'
  Text ->
  DescribeTaskSets
describeTaskSets pCluster_ pService_ =
  DescribeTaskSets'
    { _dtssTaskSets = Nothing,
      _dtssInclude = Nothing,
      _dtssCluster = pCluster_,
      _dtssService = pService_
    }

-- | The ID or full Amazon Resource Name (ARN) of task sets to describe.
dtssTaskSets :: Lens' DescribeTaskSets [Text]
dtssTaskSets = lens _dtssTaskSets (\s a -> s {_dtssTaskSets = a}) . _Default . _Coerce

-- | Specifies whether to see the resource tags for the task set. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
dtssInclude :: Lens' DescribeTaskSets [TaskSetField]
dtssInclude = lens _dtssInclude (\s a -> s {_dtssInclude = a}) . _Default . _Coerce

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task sets exist in.
dtssCluster :: Lens' DescribeTaskSets Text
dtssCluster = lens _dtssCluster (\s a -> s {_dtssCluster = a})

-- | The short name or full Amazon Resource Name (ARN) of the service that the task sets exist in.
dtssService :: Lens' DescribeTaskSets Text
dtssService = lens _dtssService (\s a -> s {_dtssService = a})

instance AWSRequest DescribeTaskSets where
  type Rs DescribeTaskSets = DescribeTaskSetsResponse
  request = postJSON ecs
  response =
    receiveJSON
      ( \s h x ->
          DescribeTaskSetsResponse'
            <$> (x .?> "taskSets" .!@ mempty)
            <*> (x .?> "failures" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeTaskSets

instance NFData DescribeTaskSets

instance ToHeaders DescribeTaskSets where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AmazonEC2ContainerServiceV20141113.DescribeTaskSets" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeTaskSets where
  toJSON DescribeTaskSets' {..} =
    object
      ( catMaybes
          [ ("taskSets" .=) <$> _dtssTaskSets,
            ("include" .=) <$> _dtssInclude,
            Just ("cluster" .= _dtssCluster),
            Just ("service" .= _dtssService)
          ]
      )

instance ToPath DescribeTaskSets where
  toPath = const "/"

instance ToQuery DescribeTaskSets where
  toQuery = const mempty

-- | /See:/ 'describeTaskSetsResponse' smart constructor.
data DescribeTaskSetsResponse = DescribeTaskSetsResponse'
  { _dtssrsTaskSets ::
      !(Maybe [TaskSet]),
    _dtssrsFailures :: !(Maybe [Failure]),
    _dtssrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTaskSetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtssrsTaskSets' - The list of task sets described.
--
-- * 'dtssrsFailures' - Any failures associated with the call.
--
-- * 'dtssrsResponseStatus' - -- | The response status code.
describeTaskSetsResponse ::
  -- | 'dtssrsResponseStatus'
  Int ->
  DescribeTaskSetsResponse
describeTaskSetsResponse pResponseStatus_ =
  DescribeTaskSetsResponse'
    { _dtssrsTaskSets = Nothing,
      _dtssrsFailures = Nothing,
      _dtssrsResponseStatus = pResponseStatus_
    }

-- | The list of task sets described.
dtssrsTaskSets :: Lens' DescribeTaskSetsResponse [TaskSet]
dtssrsTaskSets = lens _dtssrsTaskSets (\s a -> s {_dtssrsTaskSets = a}) . _Default . _Coerce

-- | Any failures associated with the call.
dtssrsFailures :: Lens' DescribeTaskSetsResponse [Failure]
dtssrsFailures = lens _dtssrsFailures (\s a -> s {_dtssrsFailures = a}) . _Default . _Coerce

-- | -- | The response status code.
dtssrsResponseStatus :: Lens' DescribeTaskSetsResponse Int
dtssrsResponseStatus = lens _dtssrsResponseStatus (\s a -> s {_dtssrsResponseStatus = a})

instance NFData DescribeTaskSetsResponse

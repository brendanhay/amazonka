{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

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
    , dtdTaskDefinition

    -- * Response
    , DescribeTaskDefinitionResponse
    -- ** Response constructor
    , describeTaskDefinitionResponse
    -- ** Response lenses
    , descTaskDefinition
    , descStatus
    ) where

import           Network.AWS.ECS.Types
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
describeTaskDefinition pTaskDefinition =
    DescribeTaskDefinition'
    { _dtdTaskDefinition = pTaskDefinition
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
-- * 'descTaskDefinition'
--
-- * 'descStatus'
data DescribeTaskDefinitionResponse = DescribeTaskDefinitionResponse'
    { _descTaskDefinition :: !(Maybe TaskDefinition)
    , _descStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTaskDefinitionResponse' smart constructor.
describeTaskDefinitionResponse :: Int -> DescribeTaskDefinitionResponse
describeTaskDefinitionResponse pStatus =
    DescribeTaskDefinitionResponse'
    { _descTaskDefinition = Nothing
    , _descStatus = pStatus
    }

-- | The full task definition description.
descTaskDefinition :: Lens' DescribeTaskDefinitionResponse (Maybe TaskDefinition)
descTaskDefinition = lens _descTaskDefinition (\ s a -> s{_descTaskDefinition = a});

-- | FIXME: Undocumented member.
descStatus :: Lens' DescribeTaskDefinitionResponse Int
descStatus = lens _descStatus (\ s a -> s{_descStatus = a});

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
-- Module      : Network.AWS.ElasticBeanstalk.DescribeInstancesHealth
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns more detailed information about the health of the specified
-- instances (for example, CPU utilization, load average, and causes). The
-- __DescribeInstancesHealth__ operation is only available with AWS Elastic
-- Beanstalk Enhanced Health.
module Network.AWS.ElasticBeanstalk.DescribeInstancesHealth
    (
    -- * Creating a Request
      describeInstancesHealth
    , DescribeInstancesHealth
    -- * Request Lenses
    , dihNextToken
    , dihEnvironmentName
    , dihAttributeNames
    , dihEnvironmentId

    -- * Destructuring the Response
    , describeInstancesHealthResponse
    , DescribeInstancesHealthResponse
    -- * Response Lenses
    , dihrsInstanceHealthList
    , dihrsNextToken
    , dihrsRefreshedAt
    , dihrsResponseStatus
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.ElasticBeanstalk.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | See the example below to learn how to create a request body.
--
-- /See:/ 'describeInstancesHealth' smart constructor.
data DescribeInstancesHealth = DescribeInstancesHealth'
    { _dihNextToken       :: !(Maybe Text)
    , _dihEnvironmentName :: !(Maybe Text)
    , _dihAttributeNames  :: !(Maybe [InstancesHealthAttribute])
    , _dihEnvironmentId   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeInstancesHealth' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dihNextToken'
--
-- * 'dihEnvironmentName'
--
-- * 'dihAttributeNames'
--
-- * 'dihEnvironmentId'
describeInstancesHealth
    :: DescribeInstancesHealth
describeInstancesHealth =
    DescribeInstancesHealth'
    { _dihNextToken = Nothing
    , _dihEnvironmentName = Nothing
    , _dihAttributeNames = Nothing
    , _dihEnvironmentId = Nothing
    }

-- | Specifies the next token of the request.
dihNextToken :: Lens' DescribeInstancesHealth (Maybe Text)
dihNextToken = lens _dihNextToken (\ s a -> s{_dihNextToken = a});

-- | Specifies the AWS Elastic Beanstalk environment name.
dihEnvironmentName :: Lens' DescribeInstancesHealth (Maybe Text)
dihEnvironmentName = lens _dihEnvironmentName (\ s a -> s{_dihEnvironmentName = a});

-- | Specifies the response elements you wish to receive. If no attribute
-- names are specified, AWS Elastic Beanstalk only returns a list of
-- instances.
dihAttributeNames :: Lens' DescribeInstancesHealth [InstancesHealthAttribute]
dihAttributeNames = lens _dihAttributeNames (\ s a -> s{_dihAttributeNames = a}) . _Default . _Coerce;

-- | Specifies the AWS Elastic Beanstalk environment ID.
dihEnvironmentId :: Lens' DescribeInstancesHealth (Maybe Text)
dihEnvironmentId = lens _dihEnvironmentId (\ s a -> s{_dihEnvironmentId = a});

instance AWSRequest DescribeInstancesHealth where
        type Rs DescribeInstancesHealth =
             DescribeInstancesHealthResponse
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper "DescribeInstancesHealthResult"
              (\ s h x ->
                 DescribeInstancesHealthResponse' <$>
                   (x .@? "InstanceHealthList" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextToken")
                     <*> (x .@? "RefreshedAt")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeInstancesHealth

instance ToHeaders DescribeInstancesHealth where
        toHeaders = const mempty

instance ToPath DescribeInstancesHealth where
        toPath = const "/"

instance ToQuery DescribeInstancesHealth where
        toQuery DescribeInstancesHealth'{..}
          = mconcat
              ["Action" =:
                 ("DescribeInstancesHealth" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "NextToken" =: _dihNextToken,
               "EnvironmentName" =: _dihEnvironmentName,
               "AttributeNames" =:
                 toQuery
                   (toQueryList "member" <$> _dihAttributeNames),
               "EnvironmentId" =: _dihEnvironmentId]

-- | See the example below for a sample response.
--
-- /See:/ 'describeInstancesHealthResponse' smart constructor.
data DescribeInstancesHealthResponse = DescribeInstancesHealthResponse'
    { _dihrsInstanceHealthList :: !(Maybe [SingleInstanceHealth])
    , _dihrsNextToken          :: !(Maybe Text)
    , _dihrsRefreshedAt        :: !(Maybe ISO8601)
    , _dihrsResponseStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeInstancesHealthResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dihrsInstanceHealthList'
--
-- * 'dihrsNextToken'
--
-- * 'dihrsRefreshedAt'
--
-- * 'dihrsResponseStatus'
describeInstancesHealthResponse
    :: Int -- ^ 'dihrsResponseStatus'
    -> DescribeInstancesHealthResponse
describeInstancesHealthResponse pResponseStatus_ =
    DescribeInstancesHealthResponse'
    { _dihrsInstanceHealthList = Nothing
    , _dihrsNextToken = Nothing
    , _dihrsRefreshedAt = Nothing
    , _dihrsResponseStatus = pResponseStatus_
    }

-- | Contains the response body with information about the health of the
-- instance.
dihrsInstanceHealthList :: Lens' DescribeInstancesHealthResponse [SingleInstanceHealth]
dihrsInstanceHealthList = lens _dihrsInstanceHealthList (\ s a -> s{_dihrsInstanceHealthList = a}) . _Default . _Coerce;

-- | The next token.
dihrsNextToken :: Lens' DescribeInstancesHealthResponse (Maybe Text)
dihrsNextToken = lens _dihrsNextToken (\ s a -> s{_dihrsNextToken = a});

-- | The date and time the information was last refreshed.
dihrsRefreshedAt :: Lens' DescribeInstancesHealthResponse (Maybe UTCTime)
dihrsRefreshedAt = lens _dihrsRefreshedAt (\ s a -> s{_dihrsRefreshedAt = a}) . mapping _Time;

-- | The response status code.
dihrsResponseStatus :: Lens' DescribeInstancesHealthResponse Int
dihrsResponseStatus = lens _dihrsResponseStatus (\ s a -> s{_dihrsResponseStatus = a});

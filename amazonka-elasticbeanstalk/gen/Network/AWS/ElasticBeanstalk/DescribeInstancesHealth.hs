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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrives detailed information about the health of instances in your AWS Elastic Beanstalk. This operation requires <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced.html enhanced health reporting> .
--
--
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

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Parameters for a call to @DescribeInstancesHealth@ .
--
--
--
-- /See:/ 'describeInstancesHealth' smart constructor.
data DescribeInstancesHealth = DescribeInstancesHealth'
  { _dihNextToken       :: !(Maybe Text)
  , _dihEnvironmentName :: !(Maybe Text)
  , _dihAttributeNames  :: !(Maybe [InstancesHealthAttribute])
  , _dihEnvironmentId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInstancesHealth' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dihNextToken' - Specify the pagination token returned by a previous call.
--
-- * 'dihEnvironmentName' - Specify the AWS Elastic Beanstalk environment by name.
--
-- * 'dihAttributeNames' - Specifies the response elements you wish to receive. To retrieve all attributes, set to @All@ . If no attribute names are specified, returns a list of instances.
--
-- * 'dihEnvironmentId' - Specify the AWS Elastic Beanstalk environment by ID.
describeInstancesHealth
    :: DescribeInstancesHealth
describeInstancesHealth =
  DescribeInstancesHealth'
    { _dihNextToken = Nothing
    , _dihEnvironmentName = Nothing
    , _dihAttributeNames = Nothing
    , _dihEnvironmentId = Nothing
    }


-- | Specify the pagination token returned by a previous call.
dihNextToken :: Lens' DescribeInstancesHealth (Maybe Text)
dihNextToken = lens _dihNextToken (\ s a -> s{_dihNextToken = a})

-- | Specify the AWS Elastic Beanstalk environment by name.
dihEnvironmentName :: Lens' DescribeInstancesHealth (Maybe Text)
dihEnvironmentName = lens _dihEnvironmentName (\ s a -> s{_dihEnvironmentName = a})

-- | Specifies the response elements you wish to receive. To retrieve all attributes, set to @All@ . If no attribute names are specified, returns a list of instances.
dihAttributeNames :: Lens' DescribeInstancesHealth [InstancesHealthAttribute]
dihAttributeNames = lens _dihAttributeNames (\ s a -> s{_dihAttributeNames = a}) . _Default . _Coerce

-- | Specify the AWS Elastic Beanstalk environment by ID.
dihEnvironmentId :: Lens' DescribeInstancesHealth (Maybe Text)
dihEnvironmentId = lens _dihEnvironmentId (\ s a -> s{_dihEnvironmentId = a})

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

instance Hashable DescribeInstancesHealth where

instance NFData DescribeInstancesHealth where

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

-- | Detailed health information about the Amazon EC2 instances in an AWS Elastic Beanstalk environment.
--
--
--
-- /See:/ 'describeInstancesHealthResponse' smart constructor.
data DescribeInstancesHealthResponse = DescribeInstancesHealthResponse'
  { _dihrsInstanceHealthList :: !(Maybe [SingleInstanceHealth])
  , _dihrsNextToken          :: !(Maybe Text)
  , _dihrsRefreshedAt        :: !(Maybe ISO8601)
  , _dihrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInstancesHealthResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dihrsInstanceHealthList' - Detailed health information about each instance.
--
-- * 'dihrsNextToken' - Pagination token for the next page of results, if available.
--
-- * 'dihrsRefreshedAt' - The date and time that the health information was retrieved.
--
-- * 'dihrsResponseStatus' - -- | The response status code.
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


-- | Detailed health information about each instance.
dihrsInstanceHealthList :: Lens' DescribeInstancesHealthResponse [SingleInstanceHealth]
dihrsInstanceHealthList = lens _dihrsInstanceHealthList (\ s a -> s{_dihrsInstanceHealthList = a}) . _Default . _Coerce

-- | Pagination token for the next page of results, if available.
dihrsNextToken :: Lens' DescribeInstancesHealthResponse (Maybe Text)
dihrsNextToken = lens _dihrsNextToken (\ s a -> s{_dihrsNextToken = a})

-- | The date and time that the health information was retrieved.
dihrsRefreshedAt :: Lens' DescribeInstancesHealthResponse (Maybe UTCTime)
dihrsRefreshedAt = lens _dihrsRefreshedAt (\ s a -> s{_dihrsRefreshedAt = a}) . mapping _Time

-- | -- | The response status code.
dihrsResponseStatus :: Lens' DescribeInstancesHealthResponse Int
dihrsResponseStatus = lens _dihrsResponseStatus (\ s a -> s{_dihrsResponseStatus = a})

instance NFData DescribeInstancesHealthResponse where

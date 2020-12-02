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
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironmentHealth
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the overall health of the specified environment. The __DescribeEnvironmentHealth__ operation is only available with AWS Elastic Beanstalk Enhanced Health.
--
--
module Network.AWS.ElasticBeanstalk.DescribeEnvironmentHealth
    (
    -- * Creating a Request
      describeEnvironmentHealth
    , DescribeEnvironmentHealth
    -- * Request Lenses
    , dehEnvironmentName
    , dehAttributeNames
    , dehEnvironmentId

    -- * Destructuring the Response
    , describeEnvironmentHealthResponse
    , DescribeEnvironmentHealthResponse
    -- * Response Lenses
    , dehrsStatus
    , dehrsCauses
    , dehrsApplicationMetrics
    , dehrsColor
    , dehrsEnvironmentName
    , dehrsHealthStatus
    , dehrsInstancesHealth
    , dehrsRefreshedAt
    , dehrsResponseStatus
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | See the example below to learn how to create a request body.
--
--
--
-- /See:/ 'describeEnvironmentHealth' smart constructor.
data DescribeEnvironmentHealth = DescribeEnvironmentHealth'
  { _dehEnvironmentName :: !(Maybe Text)
  , _dehAttributeNames  :: !(Maybe [EnvironmentHealthAttribute])
  , _dehEnvironmentId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEnvironmentHealth' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dehEnvironmentName' - Specify the environment by name. You must specify either this or an EnvironmentName, or both.
--
-- * 'dehAttributeNames' - Specify the response elements to return. To retrieve all attributes, set to @All@ . If no attribute names are specified, returns the name of the environment.
--
-- * 'dehEnvironmentId' - Specify the environment by ID. You must specify either this or an EnvironmentName, or both.
describeEnvironmentHealth
    :: DescribeEnvironmentHealth
describeEnvironmentHealth =
  DescribeEnvironmentHealth'
    { _dehEnvironmentName = Nothing
    , _dehAttributeNames = Nothing
    , _dehEnvironmentId = Nothing
    }


-- | Specify the environment by name. You must specify either this or an EnvironmentName, or both.
dehEnvironmentName :: Lens' DescribeEnvironmentHealth (Maybe Text)
dehEnvironmentName = lens _dehEnvironmentName (\ s a -> s{_dehEnvironmentName = a})

-- | Specify the response elements to return. To retrieve all attributes, set to @All@ . If no attribute names are specified, returns the name of the environment.
dehAttributeNames :: Lens' DescribeEnvironmentHealth [EnvironmentHealthAttribute]
dehAttributeNames = lens _dehAttributeNames (\ s a -> s{_dehAttributeNames = a}) . _Default . _Coerce

-- | Specify the environment by ID. You must specify either this or an EnvironmentName, or both.
dehEnvironmentId :: Lens' DescribeEnvironmentHealth (Maybe Text)
dehEnvironmentId = lens _dehEnvironmentId (\ s a -> s{_dehEnvironmentId = a})

instance AWSRequest DescribeEnvironmentHealth where
        type Rs DescribeEnvironmentHealth =
             DescribeEnvironmentHealthResponse
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper "DescribeEnvironmentHealthResult"
              (\ s h x ->
                 DescribeEnvironmentHealthResponse' <$>
                   (x .@? "Status") <*>
                     (x .@? "Causes" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (x .@? "ApplicationMetrics")
                     <*> (x .@? "Color")
                     <*> (x .@? "EnvironmentName")
                     <*> (x .@? "HealthStatus")
                     <*> (x .@? "InstancesHealth")
                     <*> (x .@? "RefreshedAt")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeEnvironmentHealth where

instance NFData DescribeEnvironmentHealth where

instance ToHeaders DescribeEnvironmentHealth where
        toHeaders = const mempty

instance ToPath DescribeEnvironmentHealth where
        toPath = const "/"

instance ToQuery DescribeEnvironmentHealth where
        toQuery DescribeEnvironmentHealth'{..}
          = mconcat
              ["Action" =:
                 ("DescribeEnvironmentHealth" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "EnvironmentName" =: _dehEnvironmentName,
               "AttributeNames" =:
                 toQuery
                   (toQueryList "member" <$> _dehAttributeNames),
               "EnvironmentId" =: _dehEnvironmentId]

-- | Health details for an AWS Elastic Beanstalk environment.
--
--
--
-- /See:/ 'describeEnvironmentHealthResponse' smart constructor.
data DescribeEnvironmentHealthResponse = DescribeEnvironmentHealthResponse'
  { _dehrsStatus             :: !(Maybe EnvironmentHealth)
  , _dehrsCauses             :: !(Maybe [Text])
  , _dehrsApplicationMetrics :: !(Maybe ApplicationMetrics)
  , _dehrsColor              :: !(Maybe Text)
  , _dehrsEnvironmentName    :: !(Maybe Text)
  , _dehrsHealthStatus       :: !(Maybe Text)
  , _dehrsInstancesHealth    :: !(Maybe InstanceHealthSummary)
  , _dehrsRefreshedAt        :: !(Maybe ISO8601)
  , _dehrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEnvironmentHealthResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dehrsStatus' - The environment's operational status. @Ready@ , @Launching@ , @Updating@ , @Terminating@ , or @Terminated@ .
--
-- * 'dehrsCauses' - Descriptions of the data that contributed to the environment's current health status.
--
-- * 'dehrsApplicationMetrics' - Application request metrics for the environment.
--
-- * 'dehrsColor' - The <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health color> of the environment.
--
-- * 'dehrsEnvironmentName' - The environment's name.
--
-- * 'dehrsHealthStatus' - The <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health status> of the environment. For example, @Ok@ .
--
-- * 'dehrsInstancesHealth' - Summary health information for the instances in the environment.
--
-- * 'dehrsRefreshedAt' - The date and time that the health information was retrieved.
--
-- * 'dehrsResponseStatus' - -- | The response status code.
describeEnvironmentHealthResponse
    :: Int -- ^ 'dehrsResponseStatus'
    -> DescribeEnvironmentHealthResponse
describeEnvironmentHealthResponse pResponseStatus_ =
  DescribeEnvironmentHealthResponse'
    { _dehrsStatus = Nothing
    , _dehrsCauses = Nothing
    , _dehrsApplicationMetrics = Nothing
    , _dehrsColor = Nothing
    , _dehrsEnvironmentName = Nothing
    , _dehrsHealthStatus = Nothing
    , _dehrsInstancesHealth = Nothing
    , _dehrsRefreshedAt = Nothing
    , _dehrsResponseStatus = pResponseStatus_
    }


-- | The environment's operational status. @Ready@ , @Launching@ , @Updating@ , @Terminating@ , or @Terminated@ .
dehrsStatus :: Lens' DescribeEnvironmentHealthResponse (Maybe EnvironmentHealth)
dehrsStatus = lens _dehrsStatus (\ s a -> s{_dehrsStatus = a})

-- | Descriptions of the data that contributed to the environment's current health status.
dehrsCauses :: Lens' DescribeEnvironmentHealthResponse [Text]
dehrsCauses = lens _dehrsCauses (\ s a -> s{_dehrsCauses = a}) . _Default . _Coerce

-- | Application request metrics for the environment.
dehrsApplicationMetrics :: Lens' DescribeEnvironmentHealthResponse (Maybe ApplicationMetrics)
dehrsApplicationMetrics = lens _dehrsApplicationMetrics (\ s a -> s{_dehrsApplicationMetrics = a})

-- | The <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health color> of the environment.
dehrsColor :: Lens' DescribeEnvironmentHealthResponse (Maybe Text)
dehrsColor = lens _dehrsColor (\ s a -> s{_dehrsColor = a})

-- | The environment's name.
dehrsEnvironmentName :: Lens' DescribeEnvironmentHealthResponse (Maybe Text)
dehrsEnvironmentName = lens _dehrsEnvironmentName (\ s a -> s{_dehrsEnvironmentName = a})

-- | The <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health status> of the environment. For example, @Ok@ .
dehrsHealthStatus :: Lens' DescribeEnvironmentHealthResponse (Maybe Text)
dehrsHealthStatus = lens _dehrsHealthStatus (\ s a -> s{_dehrsHealthStatus = a})

-- | Summary health information for the instances in the environment.
dehrsInstancesHealth :: Lens' DescribeEnvironmentHealthResponse (Maybe InstanceHealthSummary)
dehrsInstancesHealth = lens _dehrsInstancesHealth (\ s a -> s{_dehrsInstancesHealth = a})

-- | The date and time that the health information was retrieved.
dehrsRefreshedAt :: Lens' DescribeEnvironmentHealthResponse (Maybe UTCTime)
dehrsRefreshedAt = lens _dehrsRefreshedAt (\ s a -> s{_dehrsRefreshedAt = a}) . mapping _Time

-- | -- | The response status code.
dehrsResponseStatus :: Lens' DescribeEnvironmentHealthResponse Int
dehrsResponseStatus = lens _dehrsResponseStatus (\ s a -> s{_dehrsResponseStatus = a})

instance NFData DescribeEnvironmentHealthResponse
         where

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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the overall health of the specified
-- environment. The __DescribeEnvironmentHealth__ operation is only
-- available with AWS Elastic Beanstalk Enhanced Health.
--
-- /See:/ <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DescribeEnvironmentHealth.html AWS API Reference> for DescribeEnvironmentHealth.
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
    , dehrsCauses
    , dehrsApplicationMetrics
    , dehrsColor
    , dehrsEnvironmentName
    , dehrsHealthStatus
    , dehrsInstancesHealth
    , dehrsRefreshedAt
    , dehrsStatus
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.ElasticBeanstalk.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | See the example below to learn how to create a request body.
--
-- /See:/ 'describeEnvironmentHealth' smart constructor.
data DescribeEnvironmentHealth = DescribeEnvironmentHealth'
    { _dehEnvironmentName :: !(Maybe Text)
    , _dehAttributeNames  :: !(Maybe [EnvironmentHealthAttribute])
    , _dehEnvironmentId   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeEnvironmentHealth' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dehEnvironmentName'
--
-- * 'dehAttributeNames'
--
-- * 'dehEnvironmentId'
describeEnvironmentHealth
    :: DescribeEnvironmentHealth
describeEnvironmentHealth =
    DescribeEnvironmentHealth'
    { _dehEnvironmentName = Nothing
    , _dehAttributeNames = Nothing
    , _dehEnvironmentId = Nothing
    }

-- | Specifies the AWS Elastic Beanstalk environment name.
dehEnvironmentName :: Lens' DescribeEnvironmentHealth (Maybe Text)
dehEnvironmentName = lens _dehEnvironmentName (\ s a -> s{_dehEnvironmentName = a});

-- | Specifies the response elements you wish to receive. If no attribute
-- names are specified, AWS Elastic Beanstalk returns all response
-- elements.
dehAttributeNames :: Lens' DescribeEnvironmentHealth [EnvironmentHealthAttribute]
dehAttributeNames = lens _dehAttributeNames (\ s a -> s{_dehAttributeNames = a}) . _Default . _Coerce;

-- | Specifies the AWS Elastic Beanstalk environment ID.
dehEnvironmentId :: Lens' DescribeEnvironmentHealth (Maybe Text)
dehEnvironmentId = lens _dehEnvironmentId (\ s a -> s{_dehEnvironmentId = a});

instance AWSRequest DescribeEnvironmentHealth where
        type Rs DescribeEnvironmentHealth =
             DescribeEnvironmentHealthResponse
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper "DescribeEnvironmentHealthResult"
              (\ s h x ->
                 DescribeEnvironmentHealthResponse' <$>
                   (x .@? "Causes" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "ApplicationMetrics")
                     <*> (x .@? "Color")
                     <*> (x .@? "EnvironmentName")
                     <*> (x .@? "HealthStatus")
                     <*> (x .@? "InstancesHealth")
                     <*> (x .@? "RefreshedAt")
                     <*> (pure (fromEnum s)))

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

-- | See the example below for a sample response.
--
-- /See:/ 'describeEnvironmentHealthResponse' smart constructor.
data DescribeEnvironmentHealthResponse = DescribeEnvironmentHealthResponse'
    { _dehrsCauses             :: !(Maybe [Text])
    , _dehrsApplicationMetrics :: !(Maybe ApplicationMetrics)
    , _dehrsColor              :: !(Maybe Text)
    , _dehrsEnvironmentName    :: !(Maybe Text)
    , _dehrsHealthStatus       :: !(Maybe Text)
    , _dehrsInstancesHealth    :: !(Maybe InstanceHealthSummary)
    , _dehrsRefreshedAt        :: !(Maybe ISO8601)
    , _dehrsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeEnvironmentHealthResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dehrsCauses'
--
-- * 'dehrsApplicationMetrics'
--
-- * 'dehrsColor'
--
-- * 'dehrsEnvironmentName'
--
-- * 'dehrsHealthStatus'
--
-- * 'dehrsInstancesHealth'
--
-- * 'dehrsRefreshedAt'
--
-- * 'dehrsStatus'
describeEnvironmentHealthResponse
    :: Int -- ^ 'dehrsStatus'
    -> DescribeEnvironmentHealthResponse
describeEnvironmentHealthResponse pStatus_ =
    DescribeEnvironmentHealthResponse'
    { _dehrsCauses = Nothing
    , _dehrsApplicationMetrics = Nothing
    , _dehrsColor = Nothing
    , _dehrsEnvironmentName = Nothing
    , _dehrsHealthStatus = Nothing
    , _dehrsInstancesHealth = Nothing
    , _dehrsRefreshedAt = Nothing
    , _dehrsStatus = pStatus_
    }

-- | Returns potential causes for the reported status.
dehrsCauses :: Lens' DescribeEnvironmentHealthResponse [Text]
dehrsCauses = lens _dehrsCauses (\ s a -> s{_dehrsCauses = a}) . _Default . _Coerce;

-- | Undocumented member.
dehrsApplicationMetrics :: Lens' DescribeEnvironmentHealthResponse (Maybe ApplicationMetrics)
dehrsApplicationMetrics = lens _dehrsApplicationMetrics (\ s a -> s{_dehrsApplicationMetrics = a});

-- | Returns the color indicator that tells you information about the health
-- of the environment. For more information, see
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses>.
dehrsColor :: Lens' DescribeEnvironmentHealthResponse (Maybe Text)
dehrsColor = lens _dehrsColor (\ s a -> s{_dehrsColor = a});

-- | The AWS Elastic Beanstalk environment name.
dehrsEnvironmentName :: Lens' DescribeEnvironmentHealthResponse (Maybe Text)
dehrsEnvironmentName = lens _dehrsEnvironmentName (\ s a -> s{_dehrsEnvironmentName = a});

-- | Contains the response body with information about the health of the
-- environment.
dehrsHealthStatus :: Lens' DescribeEnvironmentHealthResponse (Maybe Text)
dehrsHealthStatus = lens _dehrsHealthStatus (\ s a -> s{_dehrsHealthStatus = a});

-- | Undocumented member.
dehrsInstancesHealth :: Lens' DescribeEnvironmentHealthResponse (Maybe InstanceHealthSummary)
dehrsInstancesHealth = lens _dehrsInstancesHealth (\ s a -> s{_dehrsInstancesHealth = a});

-- | The date and time the information was last refreshed.
dehrsRefreshedAt :: Lens' DescribeEnvironmentHealthResponse (Maybe UTCTime)
dehrsRefreshedAt = lens _dehrsRefreshedAt (\ s a -> s{_dehrsRefreshedAt = a}) . mapping _Time;

-- | The response status code.
dehrsStatus :: Lens' DescribeEnvironmentHealthResponse Int
dehrsStatus = lens _dehrsStatus (\ s a -> s{_dehrsStatus = a});

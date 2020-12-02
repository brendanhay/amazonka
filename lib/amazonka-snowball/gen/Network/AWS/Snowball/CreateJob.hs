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
-- Module      : Network.AWS.Snowball.CreateJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a job to import or export data between Amazon S3 and your on-premises data center. Your AWS account must have the right trust policies and permissions in place to create a job for Snowball. If you're creating a job for a node in a cluster, you only need to provide the @clusterId@ value; the other job attributes are inherited from the cluster.
--
--
module Network.AWS.Snowball.CreateJob
    (
    -- * Creating a Request
      createJob
    , CreateJob
    -- * Request Lenses
    , cjJobType
    , cjKMSKeyARN
    , cjNotification
    , cjForwardingAddressId
    , cjAddressId
    , cjSnowballType
    , cjShippingOption
    , cjResources
    , cjClusterId
    , cjDescription
    , cjRoleARN
    , cjSnowballCapacityPreference

    -- * Destructuring the Response
    , createJobResponse
    , CreateJobResponse
    -- * Response Lenses
    , cjrsJobId
    , cjrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Snowball.Types
import Network.AWS.Snowball.Types.Product

-- | /See:/ 'createJob' smart constructor.
data CreateJob = CreateJob'
  { _cjJobType                    :: !(Maybe JobType)
  , _cjKMSKeyARN                  :: !(Maybe Text)
  , _cjNotification               :: !(Maybe Notification)
  , _cjForwardingAddressId        :: !(Maybe Text)
  , _cjAddressId                  :: !(Maybe Text)
  , _cjSnowballType               :: !(Maybe SnowballType)
  , _cjShippingOption             :: !(Maybe ShippingOption)
  , _cjResources                  :: !(Maybe JobResource)
  , _cjClusterId                  :: !(Maybe Text)
  , _cjDescription                :: !(Maybe Text)
  , _cjRoleARN                    :: !(Maybe Text)
  , _cjSnowballCapacityPreference :: !(Maybe SnowballCapacity)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjJobType' - Defines the type of job that you're creating.
--
-- * 'cjKMSKeyARN' - The @KmsKeyARN@ that you want to associate with this job. @KmsKeyARN@ s are created using the <http://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> AWS Key Management Service (KMS) API action.
--
-- * 'cjNotification' - Defines the Amazon Simple Notification Service (Amazon SNS) notification settings for this job.
--
-- * 'cjForwardingAddressId' - The forwarding address ID for a job. This field is not supported in most regions.
--
-- * 'cjAddressId' - The ID for the address that you want the Snowball shipped to.
--
-- * 'cjSnowballType' - The type of AWS Snowball appliance to use for this job. Currently, the only supported appliance type for cluster jobs is @EDGE@ .
--
-- * 'cjShippingOption' - The shipping speed for this job. This speed doesn't dictate how soon you'll get the Snowball, rather it represents how quickly the Snowball moves to its destination while in transit. Regional shipping speeds are as follows:     * In Australia, you have access to express shipping. Typically, Snowballs shipped express are delivered in about a day.     * In the European Union (EU), you have access to express shipping. Typically, Snowballs shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.     * In India, Snowballs are delivered in one to seven days.     * In the US, you have access to one-day shipping and two-day shipping.
--
-- * 'cjResources' - Defines the Amazon S3 buckets associated with this job. With @IMPORT@ jobs, you specify the bucket or buckets that your transferred data will be imported into. With @EXPORT@ jobs, you specify the bucket or buckets that your transferred data will be exported from. Optionally, you can also specify a @KeyRange@ value. If you choose to export a range, you define the length of the range by providing either an inclusive @BeginMarker@ value, an inclusive @EndMarker@ value, or both. Ranges are UTF-8 binary sorted.
--
-- * 'cjClusterId' - The ID of a cluster. If you're creating a job for a node in a cluster, you need to provide only this @clusterId@ value. The other job attributes are inherited from the cluster.
--
-- * 'cjDescription' - Defines an optional description of this specific job, for example @Important Photos 2016-08-11@ .
--
-- * 'cjRoleARN' - The @RoleARN@ that you want to associate with this job. @RoleArn@ s are created using the <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> AWS Identity and Access Management (IAM) API action.
--
-- * 'cjSnowballCapacityPreference' - If your job is being created in one of the US regions, you have the option of specifying what size Snowball you'd like for this job. In all other regions, Snowballs come with 80 TB in storage capacity.
createJob
    :: CreateJob
createJob =
  CreateJob'
    { _cjJobType = Nothing
    , _cjKMSKeyARN = Nothing
    , _cjNotification = Nothing
    , _cjForwardingAddressId = Nothing
    , _cjAddressId = Nothing
    , _cjSnowballType = Nothing
    , _cjShippingOption = Nothing
    , _cjResources = Nothing
    , _cjClusterId = Nothing
    , _cjDescription = Nothing
    , _cjRoleARN = Nothing
    , _cjSnowballCapacityPreference = Nothing
    }


-- | Defines the type of job that you're creating.
cjJobType :: Lens' CreateJob (Maybe JobType)
cjJobType = lens _cjJobType (\ s a -> s{_cjJobType = a})

-- | The @KmsKeyARN@ that you want to associate with this job. @KmsKeyARN@ s are created using the <http://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> AWS Key Management Service (KMS) API action.
cjKMSKeyARN :: Lens' CreateJob (Maybe Text)
cjKMSKeyARN = lens _cjKMSKeyARN (\ s a -> s{_cjKMSKeyARN = a})

-- | Defines the Amazon Simple Notification Service (Amazon SNS) notification settings for this job.
cjNotification :: Lens' CreateJob (Maybe Notification)
cjNotification = lens _cjNotification (\ s a -> s{_cjNotification = a})

-- | The forwarding address ID for a job. This field is not supported in most regions.
cjForwardingAddressId :: Lens' CreateJob (Maybe Text)
cjForwardingAddressId = lens _cjForwardingAddressId (\ s a -> s{_cjForwardingAddressId = a})

-- | The ID for the address that you want the Snowball shipped to.
cjAddressId :: Lens' CreateJob (Maybe Text)
cjAddressId = lens _cjAddressId (\ s a -> s{_cjAddressId = a})

-- | The type of AWS Snowball appliance to use for this job. Currently, the only supported appliance type for cluster jobs is @EDGE@ .
cjSnowballType :: Lens' CreateJob (Maybe SnowballType)
cjSnowballType = lens _cjSnowballType (\ s a -> s{_cjSnowballType = a})

-- | The shipping speed for this job. This speed doesn't dictate how soon you'll get the Snowball, rather it represents how quickly the Snowball moves to its destination while in transit. Regional shipping speeds are as follows:     * In Australia, you have access to express shipping. Typically, Snowballs shipped express are delivered in about a day.     * In the European Union (EU), you have access to express shipping. Typically, Snowballs shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.     * In India, Snowballs are delivered in one to seven days.     * In the US, you have access to one-day shipping and two-day shipping.
cjShippingOption :: Lens' CreateJob (Maybe ShippingOption)
cjShippingOption = lens _cjShippingOption (\ s a -> s{_cjShippingOption = a})

-- | Defines the Amazon S3 buckets associated with this job. With @IMPORT@ jobs, you specify the bucket or buckets that your transferred data will be imported into. With @EXPORT@ jobs, you specify the bucket or buckets that your transferred data will be exported from. Optionally, you can also specify a @KeyRange@ value. If you choose to export a range, you define the length of the range by providing either an inclusive @BeginMarker@ value, an inclusive @EndMarker@ value, or both. Ranges are UTF-8 binary sorted.
cjResources :: Lens' CreateJob (Maybe JobResource)
cjResources = lens _cjResources (\ s a -> s{_cjResources = a})

-- | The ID of a cluster. If you're creating a job for a node in a cluster, you need to provide only this @clusterId@ value. The other job attributes are inherited from the cluster.
cjClusterId :: Lens' CreateJob (Maybe Text)
cjClusterId = lens _cjClusterId (\ s a -> s{_cjClusterId = a})

-- | Defines an optional description of this specific job, for example @Important Photos 2016-08-11@ .
cjDescription :: Lens' CreateJob (Maybe Text)
cjDescription = lens _cjDescription (\ s a -> s{_cjDescription = a})

-- | The @RoleARN@ that you want to associate with this job. @RoleArn@ s are created using the <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> AWS Identity and Access Management (IAM) API action.
cjRoleARN :: Lens' CreateJob (Maybe Text)
cjRoleARN = lens _cjRoleARN (\ s a -> s{_cjRoleARN = a})

-- | If your job is being created in one of the US regions, you have the option of specifying what size Snowball you'd like for this job. In all other regions, Snowballs come with 80 TB in storage capacity.
cjSnowballCapacityPreference :: Lens' CreateJob (Maybe SnowballCapacity)
cjSnowballCapacityPreference = lens _cjSnowballCapacityPreference (\ s a -> s{_cjSnowballCapacityPreference = a})

instance AWSRequest CreateJob where
        type Rs CreateJob = CreateJobResponse
        request = postJSON snowball
        response
          = receiveJSON
              (\ s h x ->
                 CreateJobResponse' <$>
                   (x .?> "JobId") <*> (pure (fromEnum s)))

instance Hashable CreateJob where

instance NFData CreateJob where

instance ToHeaders CreateJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSIESnowballJobManagementService.CreateJob" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateJob where
        toJSON CreateJob'{..}
          = object
              (catMaybes
                 [("JobType" .=) <$> _cjJobType,
                  ("KmsKeyARN" .=) <$> _cjKMSKeyARN,
                  ("Notification" .=) <$> _cjNotification,
                  ("ForwardingAddressId" .=) <$>
                    _cjForwardingAddressId,
                  ("AddressId" .=) <$> _cjAddressId,
                  ("SnowballType" .=) <$> _cjSnowballType,
                  ("ShippingOption" .=) <$> _cjShippingOption,
                  ("Resources" .=) <$> _cjResources,
                  ("ClusterId" .=) <$> _cjClusterId,
                  ("Description" .=) <$> _cjDescription,
                  ("RoleARN" .=) <$> _cjRoleARN,
                  ("SnowballCapacityPreference" .=) <$>
                    _cjSnowballCapacityPreference])

instance ToPath CreateJob where
        toPath = const "/"

instance ToQuery CreateJob where
        toQuery = const mempty

-- | /See:/ 'createJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { _cjrsJobId          :: !(Maybe Text)
  , _cjrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjrsJobId' - The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- * 'cjrsResponseStatus' - -- | The response status code.
createJobResponse
    :: Int -- ^ 'cjrsResponseStatus'
    -> CreateJobResponse
createJobResponse pResponseStatus_ =
  CreateJobResponse'
    {_cjrsJobId = Nothing, _cjrsResponseStatus = pResponseStatus_}


-- | The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
cjrsJobId :: Lens' CreateJobResponse (Maybe Text)
cjrsJobId = lens _cjrsJobId (\ s a -> s{_cjrsJobId = a})

-- | -- | The response status code.
cjrsResponseStatus :: Lens' CreateJobResponse Int
cjrsResponseStatus = lens _cjrsResponseStatus (\ s a -> s{_cjrsResponseStatus = a})

instance NFData CreateJobResponse where

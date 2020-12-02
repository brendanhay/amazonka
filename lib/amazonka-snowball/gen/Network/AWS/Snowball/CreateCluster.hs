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
-- Module      : Network.AWS.Snowball.CreateCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an empty cluster. Each cluster supports five nodes. You use the 'CreateJob' action separately to create the jobs for each of these nodes. The cluster does not ship until these five node jobs have been created.
--
--
module Network.AWS.Snowball.CreateCluster
    (
    -- * Creating a Request
      createCluster
    , CreateCluster
    -- * Request Lenses
    , ccKMSKeyARN
    , ccNotification
    , ccForwardingAddressId
    , ccSnowballType
    , ccDescription
    , ccJobType
    , ccResources
    , ccAddressId
    , ccRoleARN
    , ccShippingOption

    -- * Destructuring the Response
    , createClusterResponse
    , CreateClusterResponse
    -- * Response Lenses
    , crersClusterId
    , crersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Snowball.Types
import Network.AWS.Snowball.Types.Product

-- | /See:/ 'createCluster' smart constructor.
data CreateCluster = CreateCluster'
  { _ccKMSKeyARN           :: !(Maybe Text)
  , _ccNotification        :: !(Maybe Notification)
  , _ccForwardingAddressId :: !(Maybe Text)
  , _ccSnowballType        :: !(Maybe SnowballType)
  , _ccDescription         :: !(Maybe Text)
  , _ccJobType             :: !JobType
  , _ccResources           :: !JobResource
  , _ccAddressId           :: !Text
  , _ccRoleARN             :: !Text
  , _ccShippingOption      :: !ShippingOption
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccKMSKeyARN' - The @KmsKeyARN@ value that you want to associate with this cluster. @KmsKeyARN@ values are created by using the <http://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> API action in AWS Key Management Service (AWS KMS).
--
-- * 'ccNotification' - The Amazon Simple Notification Service (Amazon SNS) notification settings for this cluster.
--
-- * 'ccForwardingAddressId' - The forwarding address ID for a cluster. This field is not supported in most regions.
--
-- * 'ccSnowballType' - The type of AWS Snowball appliance to use for this cluster. Currently, the only supported appliance type for cluster jobs is @EDGE@ .
--
-- * 'ccDescription' - An optional description of this specific cluster, for example @Environmental Data Cluster-01@ .
--
-- * 'ccJobType' - The type of job for this cluster. Currently, the only job type supported for clusters is @LOCAL_USE@ .
--
-- * 'ccResources' - The resources associated with the cluster job. These resources include Amazon S3 buckets and optional AWS Lambda functions written in the Python language.
--
-- * 'ccAddressId' - The ID for the address that you want the cluster shipped to.
--
-- * 'ccRoleARN' - The @RoleARN@ that you want to associate with this cluster. @RoleArn@ values are created by using the <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
--
-- * 'ccShippingOption' - The shipping speed for each node in this cluster. This speed doesn't dictate how soon you'll get each Snowball Edge appliance, rather it represents how quickly each appliance moves to its destination while in transit. Regional shipping speeds are as follows:     * In Australia, you have access to express shipping. Typically, appliances shipped express are delivered in about a day.     * In the European Union (EU), you have access to express shipping. Typically, Snowball Edges shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.     * In India, Snowball Edges are delivered in one to seven days.     * In the US, you have access to one-day shipping and two-day shipping.
createCluster
    :: JobType -- ^ 'ccJobType'
    -> JobResource -- ^ 'ccResources'
    -> Text -- ^ 'ccAddressId'
    -> Text -- ^ 'ccRoleARN'
    -> ShippingOption -- ^ 'ccShippingOption'
    -> CreateCluster
createCluster pJobType_ pResources_ pAddressId_ pRoleARN_ pShippingOption_ =
  CreateCluster'
    { _ccKMSKeyARN = Nothing
    , _ccNotification = Nothing
    , _ccForwardingAddressId = Nothing
    , _ccSnowballType = Nothing
    , _ccDescription = Nothing
    , _ccJobType = pJobType_
    , _ccResources = pResources_
    , _ccAddressId = pAddressId_
    , _ccRoleARN = pRoleARN_
    , _ccShippingOption = pShippingOption_
    }


-- | The @KmsKeyARN@ value that you want to associate with this cluster. @KmsKeyARN@ values are created by using the <http://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> API action in AWS Key Management Service (AWS KMS).
ccKMSKeyARN :: Lens' CreateCluster (Maybe Text)
ccKMSKeyARN = lens _ccKMSKeyARN (\ s a -> s{_ccKMSKeyARN = a})

-- | The Amazon Simple Notification Service (Amazon SNS) notification settings for this cluster.
ccNotification :: Lens' CreateCluster (Maybe Notification)
ccNotification = lens _ccNotification (\ s a -> s{_ccNotification = a})

-- | The forwarding address ID for a cluster. This field is not supported in most regions.
ccForwardingAddressId :: Lens' CreateCluster (Maybe Text)
ccForwardingAddressId = lens _ccForwardingAddressId (\ s a -> s{_ccForwardingAddressId = a})

-- | The type of AWS Snowball appliance to use for this cluster. Currently, the only supported appliance type for cluster jobs is @EDGE@ .
ccSnowballType :: Lens' CreateCluster (Maybe SnowballType)
ccSnowballType = lens _ccSnowballType (\ s a -> s{_ccSnowballType = a})

-- | An optional description of this specific cluster, for example @Environmental Data Cluster-01@ .
ccDescription :: Lens' CreateCluster (Maybe Text)
ccDescription = lens _ccDescription (\ s a -> s{_ccDescription = a})

-- | The type of job for this cluster. Currently, the only job type supported for clusters is @LOCAL_USE@ .
ccJobType :: Lens' CreateCluster JobType
ccJobType = lens _ccJobType (\ s a -> s{_ccJobType = a})

-- | The resources associated with the cluster job. These resources include Amazon S3 buckets and optional AWS Lambda functions written in the Python language.
ccResources :: Lens' CreateCluster JobResource
ccResources = lens _ccResources (\ s a -> s{_ccResources = a})

-- | The ID for the address that you want the cluster shipped to.
ccAddressId :: Lens' CreateCluster Text
ccAddressId = lens _ccAddressId (\ s a -> s{_ccAddressId = a})

-- | The @RoleARN@ that you want to associate with this cluster. @RoleArn@ values are created by using the <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
ccRoleARN :: Lens' CreateCluster Text
ccRoleARN = lens _ccRoleARN (\ s a -> s{_ccRoleARN = a})

-- | The shipping speed for each node in this cluster. This speed doesn't dictate how soon you'll get each Snowball Edge appliance, rather it represents how quickly each appliance moves to its destination while in transit. Regional shipping speeds are as follows:     * In Australia, you have access to express shipping. Typically, appliances shipped express are delivered in about a day.     * In the European Union (EU), you have access to express shipping. Typically, Snowball Edges shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.     * In India, Snowball Edges are delivered in one to seven days.     * In the US, you have access to one-day shipping and two-day shipping.
ccShippingOption :: Lens' CreateCluster ShippingOption
ccShippingOption = lens _ccShippingOption (\ s a -> s{_ccShippingOption = a})

instance AWSRequest CreateCluster where
        type Rs CreateCluster = CreateClusterResponse
        request = postJSON snowball
        response
          = receiveJSON
              (\ s h x ->
                 CreateClusterResponse' <$>
                   (x .?> "ClusterId") <*> (pure (fromEnum s)))

instance Hashable CreateCluster where

instance NFData CreateCluster where

instance ToHeaders CreateCluster where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSIESnowballJobManagementService.CreateCluster" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateCluster where
        toJSON CreateCluster'{..}
          = object
              (catMaybes
                 [("KmsKeyARN" .=) <$> _ccKMSKeyARN,
                  ("Notification" .=) <$> _ccNotification,
                  ("ForwardingAddressId" .=) <$>
                    _ccForwardingAddressId,
                  ("SnowballType" .=) <$> _ccSnowballType,
                  ("Description" .=) <$> _ccDescription,
                  Just ("JobType" .= _ccJobType),
                  Just ("Resources" .= _ccResources),
                  Just ("AddressId" .= _ccAddressId),
                  Just ("RoleARN" .= _ccRoleARN),
                  Just ("ShippingOption" .= _ccShippingOption)])

instance ToPath CreateCluster where
        toPath = const "/"

instance ToQuery CreateCluster where
        toQuery = const mempty

-- | /See:/ 'createClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { _crersClusterId      :: !(Maybe Text)
  , _crersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crersClusterId' - The automatically generated ID for a cluster.
--
-- * 'crersResponseStatus' - -- | The response status code.
createClusterResponse
    :: Int -- ^ 'crersResponseStatus'
    -> CreateClusterResponse
createClusterResponse pResponseStatus_ =
  CreateClusterResponse'
    {_crersClusterId = Nothing, _crersResponseStatus = pResponseStatus_}


-- | The automatically generated ID for a cluster.
crersClusterId :: Lens' CreateClusterResponse (Maybe Text)
crersClusterId = lens _crersClusterId (\ s a -> s{_crersClusterId = a})

-- | -- | The response status code.
crersResponseStatus :: Lens' CreateClusterResponse Int
crersResponseStatus = lens _crersResponseStatus (\ s a -> s{_crersResponseStatus = a})

instance NFData CreateClusterResponse where

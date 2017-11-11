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
-- Module      : Network.AWS.CloudHSM.DescribeHSM
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an HSM. You can identify the HSM by its ARN or its serial number.
--
--
module Network.AWS.CloudHSM.DescribeHSM
    (
    -- * Creating a Request
      describeHSM
    , DescribeHSM
    -- * Request Lenses
    , dhsmHSMSerialNumber
    , dhsmHSMARN

    -- * Destructuring the Response
    , describeHSMResponse
    , DescribeHSMResponse
    -- * Response Lenses
    , desrsStatus
    , desrsIAMRoleARN
    , desrsEniId
    , desrsVPCId
    , desrsSSHKeyLastUpdated
    , desrsSubscriptionEndDate
    , desrsServerCertURI
    , desrsSubscriptionType
    , desrsSSHPublicKey
    , desrsSubnetId
    , desrsStatusDetails
    , desrsPartitions
    , desrsSubscriptionStartDate
    , desrsAvailabilityZone
    , desrsServerCertLastUpdated
    , desrsSoftwareVersion
    , desrsVendorName
    , desrsSerialNumber
    , desrsHSMARN
    , desrsEniIP
    , desrsHSMType
    , desrsResponseStatus
    ) where

import Network.AWS.CloudHSM.Types
import Network.AWS.CloudHSM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the inputs for the 'DescribeHsm' operation.
--
--
--
-- /See:/ 'describeHSM' smart constructor.
data DescribeHSM = DescribeHSM'
  { _dhsmHSMSerialNumber :: {-# NOUNPACK #-}!(Maybe Text)
  , _dhsmHSMARN          :: {-# NOUNPACK #-}!(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeHSM' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhsmHSMSerialNumber' - The serial number of the HSM. Either the /HsmArn/ or the /HsmSerialNumber/ parameter must be specified.
--
-- * 'dhsmHSMARN' - The ARN of the HSM. Either the /HsmArn/ or the /SerialNumber/ parameter must be specified.
describeHSM
    :: DescribeHSM
describeHSM =
  DescribeHSM' {_dhsmHSMSerialNumber = Nothing, _dhsmHSMARN = Nothing}


-- | The serial number of the HSM. Either the /HsmArn/ or the /HsmSerialNumber/ parameter must be specified.
dhsmHSMSerialNumber :: Lens' DescribeHSM (Maybe Text)
dhsmHSMSerialNumber = lens _dhsmHSMSerialNumber (\ s a -> s{_dhsmHSMSerialNumber = a});

-- | The ARN of the HSM. Either the /HsmArn/ or the /SerialNumber/ parameter must be specified.
dhsmHSMARN :: Lens' DescribeHSM (Maybe Text)
dhsmHSMARN = lens _dhsmHSMARN (\ s a -> s{_dhsmHSMARN = a});

instance AWSRequest DescribeHSM where
        type Rs DescribeHSM = DescribeHSMResponse
        request = postJSON cloudHSM
        response
          = receiveJSON
              (\ s h x ->
                 DescribeHSMResponse' <$>
                   (x .?> "Status") <*> (x .?> "IamRoleArn") <*>
                     (x .?> "EniId")
                     <*> (x .?> "VpcId")
                     <*> (x .?> "SshKeyLastUpdated")
                     <*> (x .?> "SubscriptionEndDate")
                     <*> (x .?> "ServerCertUri")
                     <*> (x .?> "SubscriptionType")
                     <*> (x .?> "SshPublicKey")
                     <*> (x .?> "SubnetId")
                     <*> (x .?> "StatusDetails")
                     <*> (x .?> "Partitions" .!@ mempty)
                     <*> (x .?> "SubscriptionStartDate")
                     <*> (x .?> "AvailabilityZone")
                     <*> (x .?> "ServerCertLastUpdated")
                     <*> (x .?> "SoftwareVersion")
                     <*> (x .?> "VendorName")
                     <*> (x .?> "SerialNumber")
                     <*> (x .?> "HsmArn")
                     <*> (x .?> "EniIp")
                     <*> (x .?> "HsmType")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeHSM where

instance NFData DescribeHSM where

instance ToHeaders DescribeHSM where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.DescribeHsm" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeHSM where
        toJSON DescribeHSM'{..}
          = object
              (catMaybes
                 [("HsmSerialNumber" .=) <$> _dhsmHSMSerialNumber,
                  ("HsmArn" .=) <$> _dhsmHSMARN])

instance ToPath DescribeHSM where
        toPath = const "/"

instance ToQuery DescribeHSM where
        toQuery = const mempty

-- | Contains the output of the 'DescribeHsm' operation.
--
--
--
-- /See:/ 'describeHSMResponse' smart constructor.
data DescribeHSMResponse = DescribeHSMResponse'
  { _desrsStatus                :: {-# NOUNPACK #-}!(Maybe HSMStatus)
  , _desrsIAMRoleARN            :: {-# NOUNPACK #-}!(Maybe Text)
  , _desrsEniId                 :: {-# NOUNPACK #-}!(Maybe Text)
  , _desrsVPCId                 :: {-# NOUNPACK #-}!(Maybe Text)
  , _desrsSSHKeyLastUpdated     :: {-# NOUNPACK #-}!(Maybe Text)
  , _desrsSubscriptionEndDate   :: {-# NOUNPACK #-}!(Maybe Text)
  , _desrsServerCertURI         :: {-# NOUNPACK #-}!(Maybe Text)
  , _desrsSubscriptionType      :: {-# NOUNPACK #-}!(Maybe SubscriptionType)
  , _desrsSSHPublicKey          :: {-# NOUNPACK #-}!(Maybe Text)
  , _desrsSubnetId              :: {-# NOUNPACK #-}!(Maybe Text)
  , _desrsStatusDetails         :: {-# NOUNPACK #-}!(Maybe Text)
  , _desrsPartitions            :: {-# NOUNPACK #-}!(Maybe [Text])
  , _desrsSubscriptionStartDate :: {-# NOUNPACK #-}!(Maybe Text)
  , _desrsAvailabilityZone      :: {-# NOUNPACK #-}!(Maybe Text)
  , _desrsServerCertLastUpdated :: {-# NOUNPACK #-}!(Maybe Text)
  , _desrsSoftwareVersion       :: {-# NOUNPACK #-}!(Maybe Text)
  , _desrsVendorName            :: {-# NOUNPACK #-}!(Maybe Text)
  , _desrsSerialNumber          :: {-# NOUNPACK #-}!(Maybe Text)
  , _desrsHSMARN                :: {-# NOUNPACK #-}!(Maybe Text)
  , _desrsEniIP                 :: {-# NOUNPACK #-}!(Maybe Text)
  , _desrsHSMType               :: {-# NOUNPACK #-}!(Maybe Text)
  , _desrsResponseStatus        :: {-# NOUNPACK #-}!Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeHSMResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsStatus' - The status of the HSM.
--
-- * 'desrsIAMRoleARN' - The ARN of the IAM role assigned to the HSM.
--
-- * 'desrsEniId' - The identifier of the elastic network interface (ENI) attached to the HSM.
--
-- * 'desrsVPCId' - The identifier of the VPC that the HSM is in.
--
-- * 'desrsSSHKeyLastUpdated' - The date and time that the SSH key was last updated.
--
-- * 'desrsSubscriptionEndDate' - The subscription end date.
--
-- * 'desrsServerCertURI' - The URI of the certificate server.
--
-- * 'desrsSubscriptionType' - Undocumented member.
--
-- * 'desrsSSHPublicKey' - The public SSH key.
--
-- * 'desrsSubnetId' - The identifier of the subnet that the HSM is in.
--
-- * 'desrsStatusDetails' - Contains additional information about the status of the HSM.
--
-- * 'desrsPartitions' - The list of partitions on the HSM.
--
-- * 'desrsSubscriptionStartDate' - The subscription start date.
--
-- * 'desrsAvailabilityZone' - The Availability Zone that the HSM is in.
--
-- * 'desrsServerCertLastUpdated' - The date and time that the server certificate was last updated.
--
-- * 'desrsSoftwareVersion' - The HSM software version.
--
-- * 'desrsVendorName' - The name of the HSM vendor.
--
-- * 'desrsSerialNumber' - The serial number of the HSM.
--
-- * 'desrsHSMARN' - The ARN of the HSM.
--
-- * 'desrsEniIP' - The IP address assigned to the HSM's ENI.
--
-- * 'desrsHSMType' - The HSM model type.
--
-- * 'desrsResponseStatus' - -- | The response status code.
describeHSMResponse
    :: Int -- ^ 'desrsResponseStatus'
    -> DescribeHSMResponse
describeHSMResponse pResponseStatus_ =
  DescribeHSMResponse'
  { _desrsStatus = Nothing
  , _desrsIAMRoleARN = Nothing
  , _desrsEniId = Nothing
  , _desrsVPCId = Nothing
  , _desrsSSHKeyLastUpdated = Nothing
  , _desrsSubscriptionEndDate = Nothing
  , _desrsServerCertURI = Nothing
  , _desrsSubscriptionType = Nothing
  , _desrsSSHPublicKey = Nothing
  , _desrsSubnetId = Nothing
  , _desrsStatusDetails = Nothing
  , _desrsPartitions = Nothing
  , _desrsSubscriptionStartDate = Nothing
  , _desrsAvailabilityZone = Nothing
  , _desrsServerCertLastUpdated = Nothing
  , _desrsSoftwareVersion = Nothing
  , _desrsVendorName = Nothing
  , _desrsSerialNumber = Nothing
  , _desrsHSMARN = Nothing
  , _desrsEniIP = Nothing
  , _desrsHSMType = Nothing
  , _desrsResponseStatus = pResponseStatus_
  }


-- | The status of the HSM.
desrsStatus :: Lens' DescribeHSMResponse (Maybe HSMStatus)
desrsStatus = lens _desrsStatus (\ s a -> s{_desrsStatus = a});

-- | The ARN of the IAM role assigned to the HSM.
desrsIAMRoleARN :: Lens' DescribeHSMResponse (Maybe Text)
desrsIAMRoleARN = lens _desrsIAMRoleARN (\ s a -> s{_desrsIAMRoleARN = a});

-- | The identifier of the elastic network interface (ENI) attached to the HSM.
desrsEniId :: Lens' DescribeHSMResponse (Maybe Text)
desrsEniId = lens _desrsEniId (\ s a -> s{_desrsEniId = a});

-- | The identifier of the VPC that the HSM is in.
desrsVPCId :: Lens' DescribeHSMResponse (Maybe Text)
desrsVPCId = lens _desrsVPCId (\ s a -> s{_desrsVPCId = a});

-- | The date and time that the SSH key was last updated.
desrsSSHKeyLastUpdated :: Lens' DescribeHSMResponse (Maybe Text)
desrsSSHKeyLastUpdated = lens _desrsSSHKeyLastUpdated (\ s a -> s{_desrsSSHKeyLastUpdated = a});

-- | The subscription end date.
desrsSubscriptionEndDate :: Lens' DescribeHSMResponse (Maybe Text)
desrsSubscriptionEndDate = lens _desrsSubscriptionEndDate (\ s a -> s{_desrsSubscriptionEndDate = a});

-- | The URI of the certificate server.
desrsServerCertURI :: Lens' DescribeHSMResponse (Maybe Text)
desrsServerCertURI = lens _desrsServerCertURI (\ s a -> s{_desrsServerCertURI = a});

-- | Undocumented member.
desrsSubscriptionType :: Lens' DescribeHSMResponse (Maybe SubscriptionType)
desrsSubscriptionType = lens _desrsSubscriptionType (\ s a -> s{_desrsSubscriptionType = a});

-- | The public SSH key.
desrsSSHPublicKey :: Lens' DescribeHSMResponse (Maybe Text)
desrsSSHPublicKey = lens _desrsSSHPublicKey (\ s a -> s{_desrsSSHPublicKey = a});

-- | The identifier of the subnet that the HSM is in.
desrsSubnetId :: Lens' DescribeHSMResponse (Maybe Text)
desrsSubnetId = lens _desrsSubnetId (\ s a -> s{_desrsSubnetId = a});

-- | Contains additional information about the status of the HSM.
desrsStatusDetails :: Lens' DescribeHSMResponse (Maybe Text)
desrsStatusDetails = lens _desrsStatusDetails (\ s a -> s{_desrsStatusDetails = a});

-- | The list of partitions on the HSM.
desrsPartitions :: Lens' DescribeHSMResponse [Text]
desrsPartitions = lens _desrsPartitions (\ s a -> s{_desrsPartitions = a}) . _Default . _Coerce;

-- | The subscription start date.
desrsSubscriptionStartDate :: Lens' DescribeHSMResponse (Maybe Text)
desrsSubscriptionStartDate = lens _desrsSubscriptionStartDate (\ s a -> s{_desrsSubscriptionStartDate = a});

-- | The Availability Zone that the HSM is in.
desrsAvailabilityZone :: Lens' DescribeHSMResponse (Maybe Text)
desrsAvailabilityZone = lens _desrsAvailabilityZone (\ s a -> s{_desrsAvailabilityZone = a});

-- | The date and time that the server certificate was last updated.
desrsServerCertLastUpdated :: Lens' DescribeHSMResponse (Maybe Text)
desrsServerCertLastUpdated = lens _desrsServerCertLastUpdated (\ s a -> s{_desrsServerCertLastUpdated = a});

-- | The HSM software version.
desrsSoftwareVersion :: Lens' DescribeHSMResponse (Maybe Text)
desrsSoftwareVersion = lens _desrsSoftwareVersion (\ s a -> s{_desrsSoftwareVersion = a});

-- | The name of the HSM vendor.
desrsVendorName :: Lens' DescribeHSMResponse (Maybe Text)
desrsVendorName = lens _desrsVendorName (\ s a -> s{_desrsVendorName = a});

-- | The serial number of the HSM.
desrsSerialNumber :: Lens' DescribeHSMResponse (Maybe Text)
desrsSerialNumber = lens _desrsSerialNumber (\ s a -> s{_desrsSerialNumber = a});

-- | The ARN of the HSM.
desrsHSMARN :: Lens' DescribeHSMResponse (Maybe Text)
desrsHSMARN = lens _desrsHSMARN (\ s a -> s{_desrsHSMARN = a});

-- | The IP address assigned to the HSM's ENI.
desrsEniIP :: Lens' DescribeHSMResponse (Maybe Text)
desrsEniIP = lens _desrsEniIP (\ s a -> s{_desrsEniIP = a});

-- | The HSM model type.
desrsHSMType :: Lens' DescribeHSMResponse (Maybe Text)
desrsHSMType = lens _desrsHSMType (\ s a -> s{_desrsHSMType = a});

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeHSMResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\ s a -> s{_desrsResponseStatus = a});

instance NFData DescribeHSMResponse where

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
-- Module      : Network.AWS.SageMaker.DescribeNotebookInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a notebook instance.
--
--
module Network.AWS.SageMaker.DescribeNotebookInstance
    (
    -- * Creating a Request
      describeNotebookInstance
    , DescribeNotebookInstance
    -- * Request Lenses
    , dniNotebookInstanceName

    -- * Destructuring the Response
    , describeNotebookInstanceResponse
    , DescribeNotebookInstanceResponse
    -- * Response Lenses
    , dnirsCreationTime
    , dnirsFailureReason
    , dnirsNotebookInstanceName
    , dnirsSecurityGroups
    , dnirsURL
    , dnirsLastModifiedTime
    , dnirsNetworkInterfaceId
    , dnirsSubnetId
    , dnirsInstanceType
    , dnirsKMSKeyId
    , dnirsDirectInternetAccess
    , dnirsNotebookInstanceARN
    , dnirsNotebookInstanceLifecycleConfigName
    , dnirsRoleARN
    , dnirsResponseStatus
    , dnirsNotebookInstanceStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'describeNotebookInstance' smart constructor.
newtype DescribeNotebookInstance = DescribeNotebookInstance'
  { _dniNotebookInstanceName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNotebookInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dniNotebookInstanceName' - The name of the notebook instance that you want information about.
describeNotebookInstance
    :: Text -- ^ 'dniNotebookInstanceName'
    -> DescribeNotebookInstance
describeNotebookInstance pNotebookInstanceName_ =
  DescribeNotebookInstance' {_dniNotebookInstanceName = pNotebookInstanceName_}


-- | The name of the notebook instance that you want information about.
dniNotebookInstanceName :: Lens' DescribeNotebookInstance Text
dniNotebookInstanceName = lens _dniNotebookInstanceName (\ s a -> s{_dniNotebookInstanceName = a})

instance AWSRequest DescribeNotebookInstance where
        type Rs DescribeNotebookInstance =
             DescribeNotebookInstanceResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 DescribeNotebookInstanceResponse' <$>
                   (x .?> "CreationTime") <*> (x .?> "FailureReason")
                     <*> (x .?> "NotebookInstanceName")
                     <*> (x .?> "SecurityGroups" .!@ mempty)
                     <*> (x .?> "Url")
                     <*> (x .?> "LastModifiedTime")
                     <*> (x .?> "NetworkInterfaceId")
                     <*> (x .?> "SubnetId")
                     <*> (x .?> "InstanceType")
                     <*> (x .?> "KmsKeyId")
                     <*> (x .?> "DirectInternetAccess")
                     <*> (x .?> "NotebookInstanceArn")
                     <*> (x .?> "NotebookInstanceLifecycleConfigName")
                     <*> (x .?> "RoleArn")
                     <*> (pure (fromEnum s))
                     <*> (x .:> "NotebookInstanceStatus"))

instance Hashable DescribeNotebookInstance where

instance NFData DescribeNotebookInstance where

instance ToHeaders DescribeNotebookInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DescribeNotebookInstance" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeNotebookInstance where
        toJSON DescribeNotebookInstance'{..}
          = object
              (catMaybes
                 [Just
                    ("NotebookInstanceName" .=
                       _dniNotebookInstanceName)])

instance ToPath DescribeNotebookInstance where
        toPath = const "/"

instance ToQuery DescribeNotebookInstance where
        toQuery = const mempty

-- | /See:/ 'describeNotebookInstanceResponse' smart constructor.
data DescribeNotebookInstanceResponse = DescribeNotebookInstanceResponse'
  { _dnirsCreationTime                        :: !(Maybe POSIX)
  , _dnirsFailureReason                       :: !(Maybe Text)
  , _dnirsNotebookInstanceName                :: !(Maybe Text)
  , _dnirsSecurityGroups                      :: !(Maybe [Text])
  , _dnirsURL                                 :: !(Maybe Text)
  , _dnirsLastModifiedTime                    :: !(Maybe POSIX)
  , _dnirsNetworkInterfaceId                  :: !(Maybe Text)
  , _dnirsSubnetId                            :: !(Maybe Text)
  , _dnirsInstanceType                        :: !(Maybe InstanceType)
  , _dnirsKMSKeyId                            :: !(Maybe Text)
  , _dnirsDirectInternetAccess                :: !(Maybe DirectInternetAccess)
  , _dnirsNotebookInstanceARN                 :: !(Maybe Text)
  , _dnirsNotebookInstanceLifecycleConfigName :: !(Maybe Text)
  , _dnirsRoleARN                             :: !(Maybe Text)
  , _dnirsResponseStatus                      :: !Int
  , _dnirsNotebookInstanceStatus              :: !NotebookInstanceStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNotebookInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnirsCreationTime' - A timestamp. Use this parameter to return the time when the notebook instance was created
--
-- * 'dnirsFailureReason' - If status is failed, the reason it failed.
--
-- * 'dnirsNotebookInstanceName' - Name of the Amazon SageMaker notebook instance.
--
-- * 'dnirsSecurityGroups' - The IDs of the VPC security groups.
--
-- * 'dnirsURL' - The URL that you use to connect to the Jupyter notebook that is running in your notebook instance.
--
-- * 'dnirsLastModifiedTime' - A timestamp. Use this parameter to retrieve the time when the notebook instance was last modified.
--
-- * 'dnirsNetworkInterfaceId' - Network interface IDs that Amazon SageMaker created at the time of creating the instance.
--
-- * 'dnirsSubnetId' - The ID of the VPC subnet.
--
-- * 'dnirsInstanceType' - The type of ML compute instance running on the notebook instance.
--
-- * 'dnirsKMSKeyId' - AWS KMS key ID Amazon SageMaker uses to encrypt data when storing it on the ML storage volume attached to the instance.
--
-- * 'dnirsDirectInternetAccess' - Describes whether Amazon SageMaker provides internet access to the notebook instance. If this value is set to /Disabled, he notebook instance does not have internet access, and cannot connect to Amazon SageMaker training and endpoint services/ . For more information, see 'appendix-notebook-and-internet-access' .
--
-- * 'dnirsNotebookInstanceARN' - The Amazon Resource Name (ARN) of the notebook instance.
--
-- * 'dnirsNotebookInstanceLifecycleConfigName' - Returns the name of a notebook instance lifecycle configuration. For information about notebook instance lifestyle configurations, see 'notebook-lifecycle-config' .
--
-- * 'dnirsRoleARN' - Amazon Resource Name (ARN) of the IAM role associated with the instance.
--
-- * 'dnirsResponseStatus' - -- | The response status code.
--
-- * 'dnirsNotebookInstanceStatus' - The status of the notebook instance.
describeNotebookInstanceResponse
    :: Int -- ^ 'dnirsResponseStatus'
    -> NotebookInstanceStatus -- ^ 'dnirsNotebookInstanceStatus'
    -> DescribeNotebookInstanceResponse
describeNotebookInstanceResponse pResponseStatus_ pNotebookInstanceStatus_ =
  DescribeNotebookInstanceResponse'
    { _dnirsCreationTime = Nothing
    , _dnirsFailureReason = Nothing
    , _dnirsNotebookInstanceName = Nothing
    , _dnirsSecurityGroups = Nothing
    , _dnirsURL = Nothing
    , _dnirsLastModifiedTime = Nothing
    , _dnirsNetworkInterfaceId = Nothing
    , _dnirsSubnetId = Nothing
    , _dnirsInstanceType = Nothing
    , _dnirsKMSKeyId = Nothing
    , _dnirsDirectInternetAccess = Nothing
    , _dnirsNotebookInstanceARN = Nothing
    , _dnirsNotebookInstanceLifecycleConfigName = Nothing
    , _dnirsRoleARN = Nothing
    , _dnirsResponseStatus = pResponseStatus_
    , _dnirsNotebookInstanceStatus = pNotebookInstanceStatus_
    }


-- | A timestamp. Use this parameter to return the time when the notebook instance was created
dnirsCreationTime :: Lens' DescribeNotebookInstanceResponse (Maybe UTCTime)
dnirsCreationTime = lens _dnirsCreationTime (\ s a -> s{_dnirsCreationTime = a}) . mapping _Time

-- | If status is failed, the reason it failed.
dnirsFailureReason :: Lens' DescribeNotebookInstanceResponse (Maybe Text)
dnirsFailureReason = lens _dnirsFailureReason (\ s a -> s{_dnirsFailureReason = a})

-- | Name of the Amazon SageMaker notebook instance.
dnirsNotebookInstanceName :: Lens' DescribeNotebookInstanceResponse (Maybe Text)
dnirsNotebookInstanceName = lens _dnirsNotebookInstanceName (\ s a -> s{_dnirsNotebookInstanceName = a})

-- | The IDs of the VPC security groups.
dnirsSecurityGroups :: Lens' DescribeNotebookInstanceResponse [Text]
dnirsSecurityGroups = lens _dnirsSecurityGroups (\ s a -> s{_dnirsSecurityGroups = a}) . _Default . _Coerce

-- | The URL that you use to connect to the Jupyter notebook that is running in your notebook instance.
dnirsURL :: Lens' DescribeNotebookInstanceResponse (Maybe Text)
dnirsURL = lens _dnirsURL (\ s a -> s{_dnirsURL = a})

-- | A timestamp. Use this parameter to retrieve the time when the notebook instance was last modified.
dnirsLastModifiedTime :: Lens' DescribeNotebookInstanceResponse (Maybe UTCTime)
dnirsLastModifiedTime = lens _dnirsLastModifiedTime (\ s a -> s{_dnirsLastModifiedTime = a}) . mapping _Time

-- | Network interface IDs that Amazon SageMaker created at the time of creating the instance.
dnirsNetworkInterfaceId :: Lens' DescribeNotebookInstanceResponse (Maybe Text)
dnirsNetworkInterfaceId = lens _dnirsNetworkInterfaceId (\ s a -> s{_dnirsNetworkInterfaceId = a})

-- | The ID of the VPC subnet.
dnirsSubnetId :: Lens' DescribeNotebookInstanceResponse (Maybe Text)
dnirsSubnetId = lens _dnirsSubnetId (\ s a -> s{_dnirsSubnetId = a})

-- | The type of ML compute instance running on the notebook instance.
dnirsInstanceType :: Lens' DescribeNotebookInstanceResponse (Maybe InstanceType)
dnirsInstanceType = lens _dnirsInstanceType (\ s a -> s{_dnirsInstanceType = a})

-- | AWS KMS key ID Amazon SageMaker uses to encrypt data when storing it on the ML storage volume attached to the instance.
dnirsKMSKeyId :: Lens' DescribeNotebookInstanceResponse (Maybe Text)
dnirsKMSKeyId = lens _dnirsKMSKeyId (\ s a -> s{_dnirsKMSKeyId = a})

-- | Describes whether Amazon SageMaker provides internet access to the notebook instance. If this value is set to /Disabled, he notebook instance does not have internet access, and cannot connect to Amazon SageMaker training and endpoint services/ . For more information, see 'appendix-notebook-and-internet-access' .
dnirsDirectInternetAccess :: Lens' DescribeNotebookInstanceResponse (Maybe DirectInternetAccess)
dnirsDirectInternetAccess = lens _dnirsDirectInternetAccess (\ s a -> s{_dnirsDirectInternetAccess = a})

-- | The Amazon Resource Name (ARN) of the notebook instance.
dnirsNotebookInstanceARN :: Lens' DescribeNotebookInstanceResponse (Maybe Text)
dnirsNotebookInstanceARN = lens _dnirsNotebookInstanceARN (\ s a -> s{_dnirsNotebookInstanceARN = a})

-- | Returns the name of a notebook instance lifecycle configuration. For information about notebook instance lifestyle configurations, see 'notebook-lifecycle-config' .
dnirsNotebookInstanceLifecycleConfigName :: Lens' DescribeNotebookInstanceResponse (Maybe Text)
dnirsNotebookInstanceLifecycleConfigName = lens _dnirsNotebookInstanceLifecycleConfigName (\ s a -> s{_dnirsNotebookInstanceLifecycleConfigName = a})

-- | Amazon Resource Name (ARN) of the IAM role associated with the instance.
dnirsRoleARN :: Lens' DescribeNotebookInstanceResponse (Maybe Text)
dnirsRoleARN = lens _dnirsRoleARN (\ s a -> s{_dnirsRoleARN = a})

-- | -- | The response status code.
dnirsResponseStatus :: Lens' DescribeNotebookInstanceResponse Int
dnirsResponseStatus = lens _dnirsResponseStatus (\ s a -> s{_dnirsResponseStatus = a})

-- | The status of the notebook instance.
dnirsNotebookInstanceStatus :: Lens' DescribeNotebookInstanceResponse NotebookInstanceStatus
dnirsNotebookInstanceStatus = lens _dnirsNotebookInstanceStatus (\ s a -> s{_dnirsNotebookInstanceStatus = a})

instance NFData DescribeNotebookInstanceResponse
         where

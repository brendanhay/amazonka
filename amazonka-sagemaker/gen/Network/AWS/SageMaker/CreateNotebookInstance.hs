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
-- Module      : Network.AWS.SageMaker.CreateNotebookInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon SageMaker notebook instance. A notebook instance is a machine learning (ML) compute instance running on a Jupyter notebook.
--
--
-- In a @CreateNotebookInstance@ request, specify the type of ML compute instance that you want to run. Amazon SageMaker launches the instance, installs common libraries that you can use to explore datasets for model training, and attaches an ML storage volume to the notebook instance.
--
-- Amazon SageMaker also provides a set of example notebooks. Each notebook demonstrates how to use Amazon SageMaker with a specific algorithm or with a machine learning framework.
--
-- After receiving the request, Amazon SageMaker does the following:
--
--     * Creates a network interface in the Amazon SageMaker VPC.
--
--     * (Option) If you specified @SubnetId@ , Amazon SageMaker creates a network interface in your own VPC, which is inferred from the subnet ID that you provide in the input. When creating this network interface, Amazon SageMaker attaches the security group that you specified in the request to the network interface that it creates in your VPC.
--
--     * Launches an EC2 instance of the type specified in the request in the Amazon SageMaker VPC. If you specified @SubnetId@ of your VPC, Amazon SageMaker specifies both network interfaces when launching this instance. This enables inbound traffic from your own VPC to the notebook instance, assuming that the security groups allow it.
--
--
--
-- After creating the notebook instance, Amazon SageMaker returns its Amazon Resource Name (ARN).
--
-- After Amazon SageMaker creates the notebook instance, you can connect to the Jupyter server and work in Jupyter notebooks. For example, you can write code to explore a dataset that you can use for model training, train a model, host models by creating Amazon SageMaker endpoints, and validate hosted models.
--
-- For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/how-it-works.html How It Works> .
--
module Network.AWS.SageMaker.CreateNotebookInstance
    (
    -- * Creating a Request
      createNotebookInstance
    , CreateNotebookInstance
    -- * Request Lenses
    , cniSecurityGroupIds
    , cniLifecycleConfigName
    , cniSubnetId
    , cniKMSKeyId
    , cniDirectInternetAccess
    , cniTags
    , cniNotebookInstanceName
    , cniInstanceType
    , cniRoleARN

    -- * Destructuring the Response
    , createNotebookInstanceResponse
    , CreateNotebookInstanceResponse
    -- * Response Lenses
    , cnirsNotebookInstanceARN
    , cnirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'createNotebookInstance' smart constructor.
data CreateNotebookInstance = CreateNotebookInstance'
  { _cniSecurityGroupIds     :: !(Maybe [Text])
  , _cniLifecycleConfigName  :: !(Maybe Text)
  , _cniSubnetId             :: !(Maybe Text)
  , _cniKMSKeyId             :: !(Maybe Text)
  , _cniDirectInternetAccess :: !(Maybe DirectInternetAccess)
  , _cniTags                 :: !(Maybe [Tag])
  , _cniNotebookInstanceName :: !Text
  , _cniInstanceType         :: !InstanceType
  , _cniRoleARN              :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateNotebookInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cniSecurityGroupIds' - The VPC security group IDs, in the form sg-xxxxxxxx. The security groups must be for the same VPC as specified in the subnet.
--
-- * 'cniLifecycleConfigName' - The name of a lifecycle configuration to associate with the notebook instance. For information about lifestyle configurations, see 'notebook-lifecycle-config' .
--
-- * 'cniSubnetId' - The ID of the subnet in a VPC to which you would like to have a connectivity from your ML compute instance.
--
-- * 'cniKMSKeyId' - If you provide a AWS KMS key ID, Amazon SageMaker uses it to encrypt data at rest on the ML storage volume that is attached to your notebook instance.
--
-- * 'cniDirectInternetAccess' - Sets whether Amazon SageMaker provides internet access to the notebook instance. If you set this to @Disabled@ this notebook instance will be able to access resources only in your VPC, and will not be able to connect to Amazon SageMaker training and endpoint services unless your configure a NAT Gateway in your VPC. For more information, see 'appendix-notebook-and-internet-access' . You can set the value of this parameter to @Disabled@ only if you set a value for the @SubnetId@ parameter.
--
-- * 'cniTags' - A list of tags to associate with the notebook instance. You can add tags later by using the @CreateTags@ API.
--
-- * 'cniNotebookInstanceName' - The name of the new notebook instance.
--
-- * 'cniInstanceType' - The type of ML compute instance to launch for the notebook instance.
--
-- * 'cniRoleARN' - When you send any requests to AWS resources from the notebook instance, Amazon SageMaker assumes this role to perform tasks on your behalf. You must grant this role necessary permissions so Amazon SageMaker can perform these tasks. The policy must allow the Amazon SageMaker service principal (sagemaker.amazonaws.com) permissions to assume this role. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
createNotebookInstance
    :: Text -- ^ 'cniNotebookInstanceName'
    -> InstanceType -- ^ 'cniInstanceType'
    -> Text -- ^ 'cniRoleARN'
    -> CreateNotebookInstance
createNotebookInstance pNotebookInstanceName_ pInstanceType_ pRoleARN_ =
  CreateNotebookInstance'
    { _cniSecurityGroupIds = Nothing
    , _cniLifecycleConfigName = Nothing
    , _cniSubnetId = Nothing
    , _cniKMSKeyId = Nothing
    , _cniDirectInternetAccess = Nothing
    , _cniTags = Nothing
    , _cniNotebookInstanceName = pNotebookInstanceName_
    , _cniInstanceType = pInstanceType_
    , _cniRoleARN = pRoleARN_
    }


-- | The VPC security group IDs, in the form sg-xxxxxxxx. The security groups must be for the same VPC as specified in the subnet.
cniSecurityGroupIds :: Lens' CreateNotebookInstance [Text]
cniSecurityGroupIds = lens _cniSecurityGroupIds (\ s a -> s{_cniSecurityGroupIds = a}) . _Default . _Coerce

-- | The name of a lifecycle configuration to associate with the notebook instance. For information about lifestyle configurations, see 'notebook-lifecycle-config' .
cniLifecycleConfigName :: Lens' CreateNotebookInstance (Maybe Text)
cniLifecycleConfigName = lens _cniLifecycleConfigName (\ s a -> s{_cniLifecycleConfigName = a})

-- | The ID of the subnet in a VPC to which you would like to have a connectivity from your ML compute instance.
cniSubnetId :: Lens' CreateNotebookInstance (Maybe Text)
cniSubnetId = lens _cniSubnetId (\ s a -> s{_cniSubnetId = a})

-- | If you provide a AWS KMS key ID, Amazon SageMaker uses it to encrypt data at rest on the ML storage volume that is attached to your notebook instance.
cniKMSKeyId :: Lens' CreateNotebookInstance (Maybe Text)
cniKMSKeyId = lens _cniKMSKeyId (\ s a -> s{_cniKMSKeyId = a})

-- | Sets whether Amazon SageMaker provides internet access to the notebook instance. If you set this to @Disabled@ this notebook instance will be able to access resources only in your VPC, and will not be able to connect to Amazon SageMaker training and endpoint services unless your configure a NAT Gateway in your VPC. For more information, see 'appendix-notebook-and-internet-access' . You can set the value of this parameter to @Disabled@ only if you set a value for the @SubnetId@ parameter.
cniDirectInternetAccess :: Lens' CreateNotebookInstance (Maybe DirectInternetAccess)
cniDirectInternetAccess = lens _cniDirectInternetAccess (\ s a -> s{_cniDirectInternetAccess = a})

-- | A list of tags to associate with the notebook instance. You can add tags later by using the @CreateTags@ API.
cniTags :: Lens' CreateNotebookInstance [Tag]
cniTags = lens _cniTags (\ s a -> s{_cniTags = a}) . _Default . _Coerce

-- | The name of the new notebook instance.
cniNotebookInstanceName :: Lens' CreateNotebookInstance Text
cniNotebookInstanceName = lens _cniNotebookInstanceName (\ s a -> s{_cniNotebookInstanceName = a})

-- | The type of ML compute instance to launch for the notebook instance.
cniInstanceType :: Lens' CreateNotebookInstance InstanceType
cniInstanceType = lens _cniInstanceType (\ s a -> s{_cniInstanceType = a})

-- | When you send any requests to AWS resources from the notebook instance, Amazon SageMaker assumes this role to perform tasks on your behalf. You must grant this role necessary permissions so Amazon SageMaker can perform these tasks. The policy must allow the Amazon SageMaker service principal (sagemaker.amazonaws.com) permissions to assume this role. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
cniRoleARN :: Lens' CreateNotebookInstance Text
cniRoleARN = lens _cniRoleARN (\ s a -> s{_cniRoleARN = a})

instance AWSRequest CreateNotebookInstance where
        type Rs CreateNotebookInstance =
             CreateNotebookInstanceResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 CreateNotebookInstanceResponse' <$>
                   (x .?> "NotebookInstanceArn") <*>
                     (pure (fromEnum s)))

instance Hashable CreateNotebookInstance where

instance NFData CreateNotebookInstance where

instance ToHeaders CreateNotebookInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.CreateNotebookInstance" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateNotebookInstance where
        toJSON CreateNotebookInstance'{..}
          = object
              (catMaybes
                 [("SecurityGroupIds" .=) <$> _cniSecurityGroupIds,
                  ("LifecycleConfigName" .=) <$>
                    _cniLifecycleConfigName,
                  ("SubnetId" .=) <$> _cniSubnetId,
                  ("KmsKeyId" .=) <$> _cniKMSKeyId,
                  ("DirectInternetAccess" .=) <$>
                    _cniDirectInternetAccess,
                  ("Tags" .=) <$> _cniTags,
                  Just
                    ("NotebookInstanceName" .= _cniNotebookInstanceName),
                  Just ("InstanceType" .= _cniInstanceType),
                  Just ("RoleArn" .= _cniRoleARN)])

instance ToPath CreateNotebookInstance where
        toPath = const "/"

instance ToQuery CreateNotebookInstance where
        toQuery = const mempty

-- | /See:/ 'createNotebookInstanceResponse' smart constructor.
data CreateNotebookInstanceResponse = CreateNotebookInstanceResponse'
  { _cnirsNotebookInstanceARN :: !(Maybe Text)
  , _cnirsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateNotebookInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnirsNotebookInstanceARN' - The Amazon Resource Name (ARN) of the notebook instance.
--
-- * 'cnirsResponseStatus' - -- | The response status code.
createNotebookInstanceResponse
    :: Int -- ^ 'cnirsResponseStatus'
    -> CreateNotebookInstanceResponse
createNotebookInstanceResponse pResponseStatus_ =
  CreateNotebookInstanceResponse'
    { _cnirsNotebookInstanceARN = Nothing
    , _cnirsResponseStatus = pResponseStatus_
    }


-- | The Amazon Resource Name (ARN) of the notebook instance.
cnirsNotebookInstanceARN :: Lens' CreateNotebookInstanceResponse (Maybe Text)
cnirsNotebookInstanceARN = lens _cnirsNotebookInstanceARN (\ s a -> s{_cnirsNotebookInstanceARN = a})

-- | -- | The response status code.
cnirsResponseStatus :: Lens' CreateNotebookInstanceResponse Int
cnirsResponseStatus = lens _cnirsResponseStatus (\ s a -> s{_cnirsResponseStatus = a})

instance NFData CreateNotebookInstanceResponse where

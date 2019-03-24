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
-- Module      : Network.AWS.SageMaker.UpdateNotebookInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a notebook instance. NotebookInstance updates include upgrading or downgrading the ML compute instance used for your notebook instance to accommodate changes in your workload requirements. You can also update the VPC security groups.
--
--
module Network.AWS.SageMaker.UpdateNotebookInstance
    (
    -- * Creating a Request
      updateNotebookInstance
    , UpdateNotebookInstance
    -- * Request Lenses
    , uniAcceleratorTypes
    , uniDisassociateAdditionalCodeRepositories
    , uniAdditionalCodeRepositories
    , uniLifecycleConfigName
    , uniDisassociateLifecycleConfig
    , uniDisassociateDefaultCodeRepository
    , uniInstanceType
    , uniDefaultCodeRepository
    , uniVolumeSizeInGB
    , uniRootAccess
    , uniDisassociateAcceleratorTypes
    , uniRoleARN
    , uniNotebookInstanceName

    -- * Destructuring the Response
    , updateNotebookInstanceResponse
    , UpdateNotebookInstanceResponse
    -- * Response Lenses
    , unirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'updateNotebookInstance' smart constructor.
data UpdateNotebookInstance = UpdateNotebookInstance'
  { _uniAcceleratorTypes :: !(Maybe [NotebookInstanceAcceleratorType])
  , _uniDisassociateAdditionalCodeRepositories :: !(Maybe Bool)
  , _uniAdditionalCodeRepositories :: !(Maybe [Text])
  , _uniLifecycleConfigName :: !(Maybe Text)
  , _uniDisassociateLifecycleConfig :: !(Maybe Bool)
  , _uniDisassociateDefaultCodeRepository :: !(Maybe Bool)
  , _uniInstanceType :: !(Maybe InstanceType)
  , _uniDefaultCodeRepository :: !(Maybe Text)
  , _uniVolumeSizeInGB :: !(Maybe Nat)
  , _uniRootAccess :: !(Maybe RootAccess)
  , _uniDisassociateAcceleratorTypes :: !(Maybe Bool)
  , _uniRoleARN :: !(Maybe Text)
  , _uniNotebookInstanceName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateNotebookInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uniAcceleratorTypes' - A list of the Elastic Inference (EI) instance types to associate with this notebook instance. Currently only one EI instance type can be associated with a notebook instance. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
--
-- * 'uniDisassociateAdditionalCodeRepositories' - A list of names or URLs of the default Git repositories to remove from this notebook instance.
--
-- * 'uniAdditionalCodeRepositories' - An array of up to three Git repositories to associate with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <http://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
--
-- * 'uniLifecycleConfigName' - The name of a lifecycle configuration to associate with the notebook instance. For information about lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
--
-- * 'uniDisassociateLifecycleConfig' - Set to @true@ to remove the notebook instance lifecycle configuration currently associated with the notebook instance.
--
-- * 'uniDisassociateDefaultCodeRepository' - The name or URL of the default Git repository to remove from this notebook instance.
--
-- * 'uniInstanceType' - The Amazon ML compute instance type.
--
-- * 'uniDefaultCodeRepository' - The Git repository to associate with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <http://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
--
-- * 'uniVolumeSizeInGB' - The size, in GB, of the ML storage volume to attach to the notebook instance. The default value is 5 GB.
--
-- * 'uniRootAccess' - Whether root access is enabled or disabled for users of the notebook instance. The default value is @Enabled@ .
--
-- * 'uniDisassociateAcceleratorTypes' - A list of the Elastic Inference (EI) instance types to remove from this notebook instance.
--
-- * 'uniRoleARN' - The Amazon Resource Name (ARN) of the IAM role that Amazon SageMaker can assume to access the notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
--
-- * 'uniNotebookInstanceName' - The name of the notebook instance to update.
updateNotebookInstance
    :: Text -- ^ 'uniNotebookInstanceName'
    -> UpdateNotebookInstance
updateNotebookInstance pNotebookInstanceName_ =
  UpdateNotebookInstance'
    { _uniAcceleratorTypes = Nothing
    , _uniDisassociateAdditionalCodeRepositories = Nothing
    , _uniAdditionalCodeRepositories = Nothing
    , _uniLifecycleConfigName = Nothing
    , _uniDisassociateLifecycleConfig = Nothing
    , _uniDisassociateDefaultCodeRepository = Nothing
    , _uniInstanceType = Nothing
    , _uniDefaultCodeRepository = Nothing
    , _uniVolumeSizeInGB = Nothing
    , _uniRootAccess = Nothing
    , _uniDisassociateAcceleratorTypes = Nothing
    , _uniRoleARN = Nothing
    , _uniNotebookInstanceName = pNotebookInstanceName_
    }


-- | A list of the Elastic Inference (EI) instance types to associate with this notebook instance. Currently only one EI instance type can be associated with a notebook instance. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
uniAcceleratorTypes :: Lens' UpdateNotebookInstance [NotebookInstanceAcceleratorType]
uniAcceleratorTypes = lens _uniAcceleratorTypes (\ s a -> s{_uniAcceleratorTypes = a}) . _Default . _Coerce

-- | A list of names or URLs of the default Git repositories to remove from this notebook instance.
uniDisassociateAdditionalCodeRepositories :: Lens' UpdateNotebookInstance (Maybe Bool)
uniDisassociateAdditionalCodeRepositories = lens _uniDisassociateAdditionalCodeRepositories (\ s a -> s{_uniDisassociateAdditionalCodeRepositories = a})

-- | An array of up to three Git repositories to associate with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <http://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
uniAdditionalCodeRepositories :: Lens' UpdateNotebookInstance [Text]
uniAdditionalCodeRepositories = lens _uniAdditionalCodeRepositories (\ s a -> s{_uniAdditionalCodeRepositories = a}) . _Default . _Coerce

-- | The name of a lifecycle configuration to associate with the notebook instance. For information about lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
uniLifecycleConfigName :: Lens' UpdateNotebookInstance (Maybe Text)
uniLifecycleConfigName = lens _uniLifecycleConfigName (\ s a -> s{_uniLifecycleConfigName = a})

-- | Set to @true@ to remove the notebook instance lifecycle configuration currently associated with the notebook instance.
uniDisassociateLifecycleConfig :: Lens' UpdateNotebookInstance (Maybe Bool)
uniDisassociateLifecycleConfig = lens _uniDisassociateLifecycleConfig (\ s a -> s{_uniDisassociateLifecycleConfig = a})

-- | The name or URL of the default Git repository to remove from this notebook instance.
uniDisassociateDefaultCodeRepository :: Lens' UpdateNotebookInstance (Maybe Bool)
uniDisassociateDefaultCodeRepository = lens _uniDisassociateDefaultCodeRepository (\ s a -> s{_uniDisassociateDefaultCodeRepository = a})

-- | The Amazon ML compute instance type.
uniInstanceType :: Lens' UpdateNotebookInstance (Maybe InstanceType)
uniInstanceType = lens _uniInstanceType (\ s a -> s{_uniInstanceType = a})

-- | The Git repository to associate with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <http://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
uniDefaultCodeRepository :: Lens' UpdateNotebookInstance (Maybe Text)
uniDefaultCodeRepository = lens _uniDefaultCodeRepository (\ s a -> s{_uniDefaultCodeRepository = a})

-- | The size, in GB, of the ML storage volume to attach to the notebook instance. The default value is 5 GB.
uniVolumeSizeInGB :: Lens' UpdateNotebookInstance (Maybe Natural)
uniVolumeSizeInGB = lens _uniVolumeSizeInGB (\ s a -> s{_uniVolumeSizeInGB = a}) . mapping _Nat

-- | Whether root access is enabled or disabled for users of the notebook instance. The default value is @Enabled@ .
uniRootAccess :: Lens' UpdateNotebookInstance (Maybe RootAccess)
uniRootAccess = lens _uniRootAccess (\ s a -> s{_uniRootAccess = a})

-- | A list of the Elastic Inference (EI) instance types to remove from this notebook instance.
uniDisassociateAcceleratorTypes :: Lens' UpdateNotebookInstance (Maybe Bool)
uniDisassociateAcceleratorTypes = lens _uniDisassociateAcceleratorTypes (\ s a -> s{_uniDisassociateAcceleratorTypes = a})

-- | The Amazon Resource Name (ARN) of the IAM role that Amazon SageMaker can assume to access the notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
uniRoleARN :: Lens' UpdateNotebookInstance (Maybe Text)
uniRoleARN = lens _uniRoleARN (\ s a -> s{_uniRoleARN = a})

-- | The name of the notebook instance to update.
uniNotebookInstanceName :: Lens' UpdateNotebookInstance Text
uniNotebookInstanceName = lens _uniNotebookInstanceName (\ s a -> s{_uniNotebookInstanceName = a})

instance AWSRequest UpdateNotebookInstance where
        type Rs UpdateNotebookInstance =
             UpdateNotebookInstanceResponse
        request = postJSON sageMaker
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateNotebookInstanceResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateNotebookInstance where

instance NFData UpdateNotebookInstance where

instance ToHeaders UpdateNotebookInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.UpdateNotebookInstance" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateNotebookInstance where
        toJSON UpdateNotebookInstance'{..}
          = object
              (catMaybes
                 [("AcceleratorTypes" .=) <$> _uniAcceleratorTypes,
                  ("DisassociateAdditionalCodeRepositories" .=) <$>
                    _uniDisassociateAdditionalCodeRepositories,
                  ("AdditionalCodeRepositories" .=) <$>
                    _uniAdditionalCodeRepositories,
                  ("LifecycleConfigName" .=) <$>
                    _uniLifecycleConfigName,
                  ("DisassociateLifecycleConfig" .=) <$>
                    _uniDisassociateLifecycleConfig,
                  ("DisassociateDefaultCodeRepository" .=) <$>
                    _uniDisassociateDefaultCodeRepository,
                  ("InstanceType" .=) <$> _uniInstanceType,
                  ("DefaultCodeRepository" .=) <$>
                    _uniDefaultCodeRepository,
                  ("VolumeSizeInGB" .=) <$> _uniVolumeSizeInGB,
                  ("RootAccess" .=) <$> _uniRootAccess,
                  ("DisassociateAcceleratorTypes" .=) <$>
                    _uniDisassociateAcceleratorTypes,
                  ("RoleArn" .=) <$> _uniRoleARN,
                  Just
                    ("NotebookInstanceName" .=
                       _uniNotebookInstanceName)])

instance ToPath UpdateNotebookInstance where
        toPath = const "/"

instance ToQuery UpdateNotebookInstance where
        toQuery = const mempty

-- | /See:/ 'updateNotebookInstanceResponse' smart constructor.
newtype UpdateNotebookInstanceResponse = UpdateNotebookInstanceResponse'
  { _unirsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateNotebookInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'unirsResponseStatus' - -- | The response status code.
updateNotebookInstanceResponse
    :: Int -- ^ 'unirsResponseStatus'
    -> UpdateNotebookInstanceResponse
updateNotebookInstanceResponse pResponseStatus_ =
  UpdateNotebookInstanceResponse' {_unirsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
unirsResponseStatus :: Lens' UpdateNotebookInstanceResponse Int
unirsResponseStatus = lens _unirsResponseStatus (\ s a -> s{_unirsResponseStatus = a})

instance NFData UpdateNotebookInstanceResponse where

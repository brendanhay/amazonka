{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeNotebookInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a notebook instance.
module Network.AWS.SageMaker.DescribeNotebookInstance
  ( -- * Creating a request
    DescribeNotebookInstance (..),
    mkDescribeNotebookInstance,

    -- ** Request lenses
    dNotebookInstanceName,

    -- * Destructuring the response
    DescribeNotebookInstanceResponse (..),
    mkDescribeNotebookInstanceResponse,

    -- ** Response lenses
    dnirsCreationTime,
    dnirsFailureReason,
    dnirsAcceleratorTypes,
    dnirsNotebookInstanceName,
    dnirsSecurityGroups,
    dnirsAdditionalCodeRepositories,
    dnirsURL,
    dnirsLastModifiedTime,
    dnirsNetworkInterfaceId,
    dnirsSubnetId,
    dnirsInstanceType,
    dnirsNotebookInstanceStatus,
    dnirsDefaultCodeRepository,
    dnirsVolumeSizeInGB,
    dnirsKMSKeyId,
    dnirsRootAccess,
    dnirsDirectInternetAccess,
    dnirsNotebookInstanceARN,
    dnirsNotebookInstanceLifecycleConfigName,
    dnirsRoleARN,
    dnirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeNotebookInstance' smart constructor.
newtype DescribeNotebookInstance = DescribeNotebookInstance'
  { -- | The name of the notebook instance that you want information about.
    notebookInstanceName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeNotebookInstance' with the minimum fields required to make a request.
--
-- * 'notebookInstanceName' - The name of the notebook instance that you want information about.
mkDescribeNotebookInstance ::
  -- | 'notebookInstanceName'
  Lude.Text ->
  DescribeNotebookInstance
mkDescribeNotebookInstance pNotebookInstanceName_ =
  DescribeNotebookInstance'
    { notebookInstanceName =
        pNotebookInstanceName_
    }

-- | The name of the notebook instance that you want information about.
--
-- /Note:/ Consider using 'notebookInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNotebookInstanceName :: Lens.Lens' DescribeNotebookInstance Lude.Text
dNotebookInstanceName = Lens.lens (notebookInstanceName :: DescribeNotebookInstance -> Lude.Text) (\s a -> s {notebookInstanceName = a} :: DescribeNotebookInstance)
{-# DEPRECATED dNotebookInstanceName "Use generic-lens or generic-optics with 'notebookInstanceName' instead." #-}

instance Lude.AWSRequest DescribeNotebookInstance where
  type Rs DescribeNotebookInstance = DescribeNotebookInstanceResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeNotebookInstanceResponse'
            Lude.<$> (x Lude..?> "CreationTime")
            Lude.<*> (x Lude..?> "FailureReason")
            Lude.<*> (x Lude..?> "AcceleratorTypes" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NotebookInstanceName")
            Lude.<*> (x Lude..?> "SecurityGroups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "AdditionalCodeRepositories" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Url")
            Lude.<*> (x Lude..?> "LastModifiedTime")
            Lude.<*> (x Lude..?> "NetworkInterfaceId")
            Lude.<*> (x Lude..?> "SubnetId")
            Lude.<*> (x Lude..?> "InstanceType")
            Lude.<*> (x Lude..:> "NotebookInstanceStatus")
            Lude.<*> (x Lude..?> "DefaultCodeRepository")
            Lude.<*> (x Lude..?> "VolumeSizeInGB")
            Lude.<*> (x Lude..?> "KmsKeyId")
            Lude.<*> (x Lude..?> "RootAccess")
            Lude.<*> (x Lude..?> "DirectInternetAccess")
            Lude.<*> (x Lude..?> "NotebookInstanceArn")
            Lude.<*> (x Lude..?> "NotebookInstanceLifecycleConfigName")
            Lude.<*> (x Lude..?> "RoleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeNotebookInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeNotebookInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeNotebookInstance where
  toJSON DescribeNotebookInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("NotebookInstanceName" Lude..= notebookInstanceName)]
      )

instance Lude.ToPath DescribeNotebookInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeNotebookInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeNotebookInstanceResponse' smart constructor.
data DescribeNotebookInstanceResponse = DescribeNotebookInstanceResponse'
  { -- | A timestamp. Use this parameter to return the time when the notebook instance was created
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | If status is @Failed@ , the reason it failed.
    failureReason :: Lude.Maybe Lude.Text,
    -- | A list of the Elastic Inference (EI) instance types associated with this notebook instance. Currently only one EI instance type can be associated with a notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
    acceleratorTypes :: Lude.Maybe [NotebookInstanceAcceleratorType],
    -- | The name of the Amazon SageMaker notebook instance.
    notebookInstanceName :: Lude.Maybe Lude.Text,
    -- | The IDs of the VPC security groups.
    securityGroups :: Lude.Maybe [Lude.Text],
    -- | An array of up to three Git repositories associated with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
    additionalCodeRepositories :: Lude.Maybe [Lude.Text],
    -- | The URL that you use to connect to the Jupyter notebook that is running in your notebook instance.
    url :: Lude.Maybe Lude.Text,
    -- | A timestamp. Use this parameter to retrieve the time when the notebook instance was last modified.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | The network interface IDs that Amazon SageMaker created at the time of creating the instance.
    networkInterfaceId :: Lude.Maybe Lude.Text,
    -- | The ID of the VPC subnet.
    subnetId :: Lude.Maybe Lude.Text,
    -- | The type of ML compute instance running on the notebook instance.
    instanceType :: Lude.Maybe InstanceType,
    -- | The status of the notebook instance.
    notebookInstanceStatus :: NotebookInstanceStatus,
    -- | The Git repository associated with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
    defaultCodeRepository :: Lude.Maybe Lude.Text,
    -- | The size, in GB, of the ML storage volume attached to the notebook instance.
    volumeSizeInGB :: Lude.Maybe Lude.Natural,
    -- | The AWS KMS key ID Amazon SageMaker uses to encrypt data when storing it on the ML storage volume attached to the instance.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | Whether root access is enabled or disabled for users of the notebook instance.
    rootAccess :: Lude.Maybe RootAccess,
    -- | Describes whether Amazon SageMaker provides internet access to the notebook instance. If this value is set to /Disabled/ , the notebook instance does not have internet access, and cannot connect to Amazon SageMaker training and endpoint services.
    --
    -- For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/appendix-additional-considerations.html#appendix-notebook-and-internet-access Notebook Instances Are Internet-Enabled by Default> .
    directInternetAccess :: Lude.Maybe DirectInternetAccess,
    -- | The Amazon Resource Name (ARN) of the notebook instance.
    notebookInstanceARN :: Lude.Maybe Lude.Text,
    -- | Returns the name of a notebook instance lifecycle configuration.
    --
    -- For information about notebook instance lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance>
    notebookInstanceLifecycleConfigName :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role associated with the instance.
    roleARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeNotebookInstanceResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - A timestamp. Use this parameter to return the time when the notebook instance was created
-- * 'failureReason' - If status is @Failed@ , the reason it failed.
-- * 'acceleratorTypes' - A list of the Elastic Inference (EI) instance types associated with this notebook instance. Currently only one EI instance type can be associated with a notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
-- * 'notebookInstanceName' - The name of the Amazon SageMaker notebook instance.
-- * 'securityGroups' - The IDs of the VPC security groups.
-- * 'additionalCodeRepositories' - An array of up to three Git repositories associated with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
-- * 'url' - The URL that you use to connect to the Jupyter notebook that is running in your notebook instance.
-- * 'lastModifiedTime' - A timestamp. Use this parameter to retrieve the time when the notebook instance was last modified.
-- * 'networkInterfaceId' - The network interface IDs that Amazon SageMaker created at the time of creating the instance.
-- * 'subnetId' - The ID of the VPC subnet.
-- * 'instanceType' - The type of ML compute instance running on the notebook instance.
-- * 'notebookInstanceStatus' - The status of the notebook instance.
-- * 'defaultCodeRepository' - The Git repository associated with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
-- * 'volumeSizeInGB' - The size, in GB, of the ML storage volume attached to the notebook instance.
-- * 'kmsKeyId' - The AWS KMS key ID Amazon SageMaker uses to encrypt data when storing it on the ML storage volume attached to the instance.
-- * 'rootAccess' - Whether root access is enabled or disabled for users of the notebook instance.
-- * 'directInternetAccess' - Describes whether Amazon SageMaker provides internet access to the notebook instance. If this value is set to /Disabled/ , the notebook instance does not have internet access, and cannot connect to Amazon SageMaker training and endpoint services.
--
-- For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/appendix-additional-considerations.html#appendix-notebook-and-internet-access Notebook Instances Are Internet-Enabled by Default> .
-- * 'notebookInstanceARN' - The Amazon Resource Name (ARN) of the notebook instance.
-- * 'notebookInstanceLifecycleConfigName' - Returns the name of a notebook instance lifecycle configuration.
--
-- For information about notebook instance lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance>
-- * 'roleARN' - The Amazon Resource Name (ARN) of the IAM role associated with the instance.
-- * 'responseStatus' - The response status code.
mkDescribeNotebookInstanceResponse ::
  -- | 'notebookInstanceStatus'
  NotebookInstanceStatus ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeNotebookInstanceResponse
mkDescribeNotebookInstanceResponse
  pNotebookInstanceStatus_
  pResponseStatus_ =
    DescribeNotebookInstanceResponse'
      { creationTime = Lude.Nothing,
        failureReason = Lude.Nothing,
        acceleratorTypes = Lude.Nothing,
        notebookInstanceName = Lude.Nothing,
        securityGroups = Lude.Nothing,
        additionalCodeRepositories = Lude.Nothing,
        url = Lude.Nothing,
        lastModifiedTime = Lude.Nothing,
        networkInterfaceId = Lude.Nothing,
        subnetId = Lude.Nothing,
        instanceType = Lude.Nothing,
        notebookInstanceStatus = pNotebookInstanceStatus_,
        defaultCodeRepository = Lude.Nothing,
        volumeSizeInGB = Lude.Nothing,
        kmsKeyId = Lude.Nothing,
        rootAccess = Lude.Nothing,
        directInternetAccess = Lude.Nothing,
        notebookInstanceARN = Lude.Nothing,
        notebookInstanceLifecycleConfigName = Lude.Nothing,
        roleARN = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | A timestamp. Use this parameter to return the time when the notebook instance was created
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsCreationTime :: Lens.Lens' DescribeNotebookInstanceResponse (Lude.Maybe Lude.Timestamp)
dnirsCreationTime = Lens.lens (creationTime :: DescribeNotebookInstanceResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeNotebookInstanceResponse)
{-# DEPRECATED dnirsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | If status is @Failed@ , the reason it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsFailureReason :: Lens.Lens' DescribeNotebookInstanceResponse (Lude.Maybe Lude.Text)
dnirsFailureReason = Lens.lens (failureReason :: DescribeNotebookInstanceResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: DescribeNotebookInstanceResponse)
{-# DEPRECATED dnirsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | A list of the Elastic Inference (EI) instance types associated with this notebook instance. Currently only one EI instance type can be associated with a notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
--
-- /Note:/ Consider using 'acceleratorTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsAcceleratorTypes :: Lens.Lens' DescribeNotebookInstanceResponse (Lude.Maybe [NotebookInstanceAcceleratorType])
dnirsAcceleratorTypes = Lens.lens (acceleratorTypes :: DescribeNotebookInstanceResponse -> Lude.Maybe [NotebookInstanceAcceleratorType]) (\s a -> s {acceleratorTypes = a} :: DescribeNotebookInstanceResponse)
{-# DEPRECATED dnirsAcceleratorTypes "Use generic-lens or generic-optics with 'acceleratorTypes' instead." #-}

-- | The name of the Amazon SageMaker notebook instance.
--
-- /Note:/ Consider using 'notebookInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsNotebookInstanceName :: Lens.Lens' DescribeNotebookInstanceResponse (Lude.Maybe Lude.Text)
dnirsNotebookInstanceName = Lens.lens (notebookInstanceName :: DescribeNotebookInstanceResponse -> Lude.Maybe Lude.Text) (\s a -> s {notebookInstanceName = a} :: DescribeNotebookInstanceResponse)
{-# DEPRECATED dnirsNotebookInstanceName "Use generic-lens or generic-optics with 'notebookInstanceName' instead." #-}

-- | The IDs of the VPC security groups.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsSecurityGroups :: Lens.Lens' DescribeNotebookInstanceResponse (Lude.Maybe [Lude.Text])
dnirsSecurityGroups = Lens.lens (securityGroups :: DescribeNotebookInstanceResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: DescribeNotebookInstanceResponse)
{-# DEPRECATED dnirsSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | An array of up to three Git repositories associated with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
--
-- /Note:/ Consider using 'additionalCodeRepositories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsAdditionalCodeRepositories :: Lens.Lens' DescribeNotebookInstanceResponse (Lude.Maybe [Lude.Text])
dnirsAdditionalCodeRepositories = Lens.lens (additionalCodeRepositories :: DescribeNotebookInstanceResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {additionalCodeRepositories = a} :: DescribeNotebookInstanceResponse)
{-# DEPRECATED dnirsAdditionalCodeRepositories "Use generic-lens or generic-optics with 'additionalCodeRepositories' instead." #-}

-- | The URL that you use to connect to the Jupyter notebook that is running in your notebook instance.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsURL :: Lens.Lens' DescribeNotebookInstanceResponse (Lude.Maybe Lude.Text)
dnirsURL = Lens.lens (url :: DescribeNotebookInstanceResponse -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: DescribeNotebookInstanceResponse)
{-# DEPRECATED dnirsURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | A timestamp. Use this parameter to retrieve the time when the notebook instance was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsLastModifiedTime :: Lens.Lens' DescribeNotebookInstanceResponse (Lude.Maybe Lude.Timestamp)
dnirsLastModifiedTime = Lens.lens (lastModifiedTime :: DescribeNotebookInstanceResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: DescribeNotebookInstanceResponse)
{-# DEPRECATED dnirsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The network interface IDs that Amazon SageMaker created at the time of creating the instance.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsNetworkInterfaceId :: Lens.Lens' DescribeNotebookInstanceResponse (Lude.Maybe Lude.Text)
dnirsNetworkInterfaceId = Lens.lens (networkInterfaceId :: DescribeNotebookInstanceResponse -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: DescribeNotebookInstanceResponse)
{-# DEPRECATED dnirsNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The ID of the VPC subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsSubnetId :: Lens.Lens' DescribeNotebookInstanceResponse (Lude.Maybe Lude.Text)
dnirsSubnetId = Lens.lens (subnetId :: DescribeNotebookInstanceResponse -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: DescribeNotebookInstanceResponse)
{-# DEPRECATED dnirsSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The type of ML compute instance running on the notebook instance.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsInstanceType :: Lens.Lens' DescribeNotebookInstanceResponse (Lude.Maybe InstanceType)
dnirsInstanceType = Lens.lens (instanceType :: DescribeNotebookInstanceResponse -> Lude.Maybe InstanceType) (\s a -> s {instanceType = a} :: DescribeNotebookInstanceResponse)
{-# DEPRECATED dnirsInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The status of the notebook instance.
--
-- /Note:/ Consider using 'notebookInstanceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsNotebookInstanceStatus :: Lens.Lens' DescribeNotebookInstanceResponse NotebookInstanceStatus
dnirsNotebookInstanceStatus = Lens.lens (notebookInstanceStatus :: DescribeNotebookInstanceResponse -> NotebookInstanceStatus) (\s a -> s {notebookInstanceStatus = a} :: DescribeNotebookInstanceResponse)
{-# DEPRECATED dnirsNotebookInstanceStatus "Use generic-lens or generic-optics with 'notebookInstanceStatus' instead." #-}

-- | The Git repository associated with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
--
-- /Note:/ Consider using 'defaultCodeRepository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsDefaultCodeRepository :: Lens.Lens' DescribeNotebookInstanceResponse (Lude.Maybe Lude.Text)
dnirsDefaultCodeRepository = Lens.lens (defaultCodeRepository :: DescribeNotebookInstanceResponse -> Lude.Maybe Lude.Text) (\s a -> s {defaultCodeRepository = a} :: DescribeNotebookInstanceResponse)
{-# DEPRECATED dnirsDefaultCodeRepository "Use generic-lens or generic-optics with 'defaultCodeRepository' instead." #-}

-- | The size, in GB, of the ML storage volume attached to the notebook instance.
--
-- /Note:/ Consider using 'volumeSizeInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsVolumeSizeInGB :: Lens.Lens' DescribeNotebookInstanceResponse (Lude.Maybe Lude.Natural)
dnirsVolumeSizeInGB = Lens.lens (volumeSizeInGB :: DescribeNotebookInstanceResponse -> Lude.Maybe Lude.Natural) (\s a -> s {volumeSizeInGB = a} :: DescribeNotebookInstanceResponse)
{-# DEPRECATED dnirsVolumeSizeInGB "Use generic-lens or generic-optics with 'volumeSizeInGB' instead." #-}

-- | The AWS KMS key ID Amazon SageMaker uses to encrypt data when storing it on the ML storage volume attached to the instance.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsKMSKeyId :: Lens.Lens' DescribeNotebookInstanceResponse (Lude.Maybe Lude.Text)
dnirsKMSKeyId = Lens.lens (kmsKeyId :: DescribeNotebookInstanceResponse -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: DescribeNotebookInstanceResponse)
{-# DEPRECATED dnirsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Whether root access is enabled or disabled for users of the notebook instance.
--
-- /Note:/ Consider using 'rootAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsRootAccess :: Lens.Lens' DescribeNotebookInstanceResponse (Lude.Maybe RootAccess)
dnirsRootAccess = Lens.lens (rootAccess :: DescribeNotebookInstanceResponse -> Lude.Maybe RootAccess) (\s a -> s {rootAccess = a} :: DescribeNotebookInstanceResponse)
{-# DEPRECATED dnirsRootAccess "Use generic-lens or generic-optics with 'rootAccess' instead." #-}

-- | Describes whether Amazon SageMaker provides internet access to the notebook instance. If this value is set to /Disabled/ , the notebook instance does not have internet access, and cannot connect to Amazon SageMaker training and endpoint services.
--
-- For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/appendix-additional-considerations.html#appendix-notebook-and-internet-access Notebook Instances Are Internet-Enabled by Default> .
--
-- /Note:/ Consider using 'directInternetAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsDirectInternetAccess :: Lens.Lens' DescribeNotebookInstanceResponse (Lude.Maybe DirectInternetAccess)
dnirsDirectInternetAccess = Lens.lens (directInternetAccess :: DescribeNotebookInstanceResponse -> Lude.Maybe DirectInternetAccess) (\s a -> s {directInternetAccess = a} :: DescribeNotebookInstanceResponse)
{-# DEPRECATED dnirsDirectInternetAccess "Use generic-lens or generic-optics with 'directInternetAccess' instead." #-}

-- | The Amazon Resource Name (ARN) of the notebook instance.
--
-- /Note:/ Consider using 'notebookInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsNotebookInstanceARN :: Lens.Lens' DescribeNotebookInstanceResponse (Lude.Maybe Lude.Text)
dnirsNotebookInstanceARN = Lens.lens (notebookInstanceARN :: DescribeNotebookInstanceResponse -> Lude.Maybe Lude.Text) (\s a -> s {notebookInstanceARN = a} :: DescribeNotebookInstanceResponse)
{-# DEPRECATED dnirsNotebookInstanceARN "Use generic-lens or generic-optics with 'notebookInstanceARN' instead." #-}

-- | Returns the name of a notebook instance lifecycle configuration.
--
-- For information about notebook instance lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance>
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsNotebookInstanceLifecycleConfigName :: Lens.Lens' DescribeNotebookInstanceResponse (Lude.Maybe Lude.Text)
dnirsNotebookInstanceLifecycleConfigName = Lens.lens (notebookInstanceLifecycleConfigName :: DescribeNotebookInstanceResponse -> Lude.Maybe Lude.Text) (\s a -> s {notebookInstanceLifecycleConfigName = a} :: DescribeNotebookInstanceResponse)
{-# DEPRECATED dnirsNotebookInstanceLifecycleConfigName "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigName' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role associated with the instance.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsRoleARN :: Lens.Lens' DescribeNotebookInstanceResponse (Lude.Maybe Lude.Text)
dnirsRoleARN = Lens.lens (roleARN :: DescribeNotebookInstanceResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: DescribeNotebookInstanceResponse)
{-# DEPRECATED dnirsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirsResponseStatus :: Lens.Lens' DescribeNotebookInstanceResponse Lude.Int
dnirsResponseStatus = Lens.lens (responseStatus :: DescribeNotebookInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeNotebookInstanceResponse)
{-# DEPRECATED dnirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

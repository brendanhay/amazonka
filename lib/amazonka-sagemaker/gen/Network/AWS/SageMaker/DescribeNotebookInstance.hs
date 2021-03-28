{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeNotebookInstance (..)
    , mkDescribeNotebookInstance
    -- ** Request lenses
    , dniNotebookInstanceName

    -- * Destructuring the response
    , DescribeNotebookInstanceResponse (..)
    , mkDescribeNotebookInstanceResponse
    -- ** Response lenses
    , dnirrsAcceleratorTypes
    , dnirrsAdditionalCodeRepositories
    , dnirrsCreationTime
    , dnirrsDefaultCodeRepository
    , dnirrsDirectInternetAccess
    , dnirrsFailureReason
    , dnirrsInstanceType
    , dnirrsKmsKeyId
    , dnirrsLastModifiedTime
    , dnirrsNetworkInterfaceId
    , dnirrsNotebookInstanceArn
    , dnirrsNotebookInstanceLifecycleConfigName
    , dnirrsNotebookInstanceName
    , dnirrsNotebookInstanceStatus
    , dnirrsRoleArn
    , dnirrsRootAccess
    , dnirrsSecurityGroups
    , dnirrsSubnetId
    , dnirrsUrl
    , dnirrsVolumeSizeInGB
    , dnirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeNotebookInstance' smart constructor.
newtype DescribeNotebookInstance = DescribeNotebookInstance'
  { notebookInstanceName :: Types.NotebookInstanceName
    -- ^ The name of the notebook instance that you want information about.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNotebookInstance' value with any optional fields omitted.
mkDescribeNotebookInstance
    :: Types.NotebookInstanceName -- ^ 'notebookInstanceName'
    -> DescribeNotebookInstance
mkDescribeNotebookInstance notebookInstanceName
  = DescribeNotebookInstance'{notebookInstanceName}

-- | The name of the notebook instance that you want information about.
--
-- /Note:/ Consider using 'notebookInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniNotebookInstanceName :: Lens.Lens' DescribeNotebookInstance Types.NotebookInstanceName
dniNotebookInstanceName = Lens.field @"notebookInstanceName"
{-# INLINEABLE dniNotebookInstanceName #-}
{-# DEPRECATED notebookInstanceName "Use generic-lens or generic-optics with 'notebookInstanceName' instead"  #-}

instance Core.ToQuery DescribeNotebookInstance where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeNotebookInstance where
        toHeaders DescribeNotebookInstance{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DescribeNotebookInstance")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeNotebookInstance where
        toJSON DescribeNotebookInstance{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("NotebookInstanceName" Core..= notebookInstanceName)])

instance Core.AWSRequest DescribeNotebookInstance where
        type Rs DescribeNotebookInstance = DescribeNotebookInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeNotebookInstanceResponse' Core.<$>
                   (x Core..:? "AcceleratorTypes") Core.<*>
                     x Core..:? "AdditionalCodeRepositories"
                     Core.<*> x Core..:? "CreationTime"
                     Core.<*> x Core..:? "DefaultCodeRepository"
                     Core.<*> x Core..:? "DirectInternetAccess"
                     Core.<*> x Core..:? "FailureReason"
                     Core.<*> x Core..:? "InstanceType"
                     Core.<*> x Core..:? "KmsKeyId"
                     Core.<*> x Core..:? "LastModifiedTime"
                     Core.<*> x Core..:? "NetworkInterfaceId"
                     Core.<*> x Core..:? "NotebookInstanceArn"
                     Core.<*> x Core..:? "NotebookInstanceLifecycleConfigName"
                     Core.<*> x Core..:? "NotebookInstanceName"
                     Core.<*> x Core..: "NotebookInstanceStatus"
                     Core.<*> x Core..:? "RoleArn"
                     Core.<*> x Core..:? "RootAccess"
                     Core.<*> x Core..:? "SecurityGroups"
                     Core.<*> x Core..:? "SubnetId"
                     Core.<*> x Core..:? "Url"
                     Core.<*> x Core..:? "VolumeSizeInGB"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeNotebookInstanceResponse' smart constructor.
data DescribeNotebookInstanceResponse = DescribeNotebookInstanceResponse'
  { acceleratorTypes :: Core.Maybe [Types.NotebookInstanceAcceleratorType]
    -- ^ A list of the Elastic Inference (EI) instance types associated with this notebook instance. Currently only one EI instance type can be associated with a notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
  , additionalCodeRepositories :: Core.Maybe [Types.CodeRepositoryNameOrUrl]
    -- ^ An array of up to three Git repositories associated with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A timestamp. Use this parameter to return the time when the notebook instance was created
  , defaultCodeRepository :: Core.Maybe Types.DefaultCodeRepository
    -- ^ The Git repository associated with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
  , directInternetAccess :: Core.Maybe Types.DirectInternetAccess
    -- ^ Describes whether Amazon SageMaker provides internet access to the notebook instance. If this value is set to /Disabled/ , the notebook instance does not have internet access, and cannot connect to Amazon SageMaker training and endpoint services.
--
-- For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/appendix-additional-considerations.html#appendix-notebook-and-internet-access Notebook Instances Are Internet-Enabled by Default> .
  , failureReason :: Core.Maybe Types.FailureReason
    -- ^ If status is @Failed@ , the reason it failed.
  , instanceType :: Core.Maybe Types.InstanceType
    -- ^ The type of ML compute instance running on the notebook instance.
  , kmsKeyId :: Core.Maybe Types.KmsKeyId
    -- ^ The AWS KMS key ID Amazon SageMaker uses to encrypt data when storing it on the ML storage volume attached to the instance. 
  , lastModifiedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A timestamp. Use this parameter to retrieve the time when the notebook instance was last modified. 
  , networkInterfaceId :: Core.Maybe Types.NetworkInterfaceId
    -- ^ The network interface IDs that Amazon SageMaker created at the time of creating the instance. 
  , notebookInstanceArn :: Core.Maybe Types.NotebookInstanceArn
    -- ^ The Amazon Resource Name (ARN) of the notebook instance.
  , notebookInstanceLifecycleConfigName :: Core.Maybe Types.NotebookInstanceLifecycleConfigName
    -- ^ Returns the name of a notebook instance lifecycle configuration.
--
-- For information about notebook instance lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> 
  , notebookInstanceName :: Core.Maybe Types.NotebookInstanceName
    -- ^ The name of the Amazon SageMaker notebook instance. 
  , notebookInstanceStatus :: Types.NotebookInstanceStatus
    -- ^ The status of the notebook instance.
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ The Amazon Resource Name (ARN) of the IAM role associated with the instance. 
  , rootAccess :: Core.Maybe Types.RootAccess
    -- ^ Whether root access is enabled or disabled for users of the notebook instance.
  , securityGroups :: Core.Maybe [Types.SecurityGroupId]
    -- ^ The IDs of the VPC security groups.
  , subnetId :: Core.Maybe Types.SubnetId
    -- ^ The ID of the VPC subnet.
  , url :: Core.Maybe Types.Url
    -- ^ The URL that you use to connect to the Jupyter notebook that is running in your notebook instance. 
  , volumeSizeInGB :: Core.Maybe Core.Natural
    -- ^ The size, in GB, of the ML storage volume attached to the notebook instance.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeNotebookInstanceResponse' value with any optional fields omitted.
mkDescribeNotebookInstanceResponse
    :: Types.NotebookInstanceStatus -- ^ 'notebookInstanceStatus'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeNotebookInstanceResponse
mkDescribeNotebookInstanceResponse notebookInstanceStatus
  responseStatus
  = DescribeNotebookInstanceResponse'{acceleratorTypes =
                                        Core.Nothing,
                                      additionalCodeRepositories = Core.Nothing,
                                      creationTime = Core.Nothing,
                                      defaultCodeRepository = Core.Nothing,
                                      directInternetAccess = Core.Nothing,
                                      failureReason = Core.Nothing, instanceType = Core.Nothing,
                                      kmsKeyId = Core.Nothing, lastModifiedTime = Core.Nothing,
                                      networkInterfaceId = Core.Nothing,
                                      notebookInstanceArn = Core.Nothing,
                                      notebookInstanceLifecycleConfigName = Core.Nothing,
                                      notebookInstanceName = Core.Nothing, notebookInstanceStatus,
                                      roleArn = Core.Nothing, rootAccess = Core.Nothing,
                                      securityGroups = Core.Nothing, subnetId = Core.Nothing,
                                      url = Core.Nothing, volumeSizeInGB = Core.Nothing,
                                      responseStatus}

-- | A list of the Elastic Inference (EI) instance types associated with this notebook instance. Currently only one EI instance type can be associated with a notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
--
-- /Note:/ Consider using 'acceleratorTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsAcceleratorTypes :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe [Types.NotebookInstanceAcceleratorType])
dnirrsAcceleratorTypes = Lens.field @"acceleratorTypes"
{-# INLINEABLE dnirrsAcceleratorTypes #-}
{-# DEPRECATED acceleratorTypes "Use generic-lens or generic-optics with 'acceleratorTypes' instead"  #-}

-- | An array of up to three Git repositories associated with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
--
-- /Note:/ Consider using 'additionalCodeRepositories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsAdditionalCodeRepositories :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe [Types.CodeRepositoryNameOrUrl])
dnirrsAdditionalCodeRepositories = Lens.field @"additionalCodeRepositories"
{-# INLINEABLE dnirrsAdditionalCodeRepositories #-}
{-# DEPRECATED additionalCodeRepositories "Use generic-lens or generic-optics with 'additionalCodeRepositories' instead"  #-}

-- | A timestamp. Use this parameter to return the time when the notebook instance was created
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsCreationTime :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Core.NominalDiffTime)
dnirrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE dnirrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The Git repository associated with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
--
-- /Note:/ Consider using 'defaultCodeRepository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsDefaultCodeRepository :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Types.DefaultCodeRepository)
dnirrsDefaultCodeRepository = Lens.field @"defaultCodeRepository"
{-# INLINEABLE dnirrsDefaultCodeRepository #-}
{-# DEPRECATED defaultCodeRepository "Use generic-lens or generic-optics with 'defaultCodeRepository' instead"  #-}

-- | Describes whether Amazon SageMaker provides internet access to the notebook instance. If this value is set to /Disabled/ , the notebook instance does not have internet access, and cannot connect to Amazon SageMaker training and endpoint services.
--
-- For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/appendix-additional-considerations.html#appendix-notebook-and-internet-access Notebook Instances Are Internet-Enabled by Default> .
--
-- /Note:/ Consider using 'directInternetAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsDirectInternetAccess :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Types.DirectInternetAccess)
dnirrsDirectInternetAccess = Lens.field @"directInternetAccess"
{-# INLINEABLE dnirrsDirectInternetAccess #-}
{-# DEPRECATED directInternetAccess "Use generic-lens or generic-optics with 'directInternetAccess' instead"  #-}

-- | If status is @Failed@ , the reason it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsFailureReason :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Types.FailureReason)
dnirrsFailureReason = Lens.field @"failureReason"
{-# INLINEABLE dnirrsFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | The type of ML compute instance running on the notebook instance.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsInstanceType :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Types.InstanceType)
dnirrsInstanceType = Lens.field @"instanceType"
{-# INLINEABLE dnirrsInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The AWS KMS key ID Amazon SageMaker uses to encrypt data when storing it on the ML storage volume attached to the instance. 
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsKmsKeyId :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Types.KmsKeyId)
dnirrsKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE dnirrsKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | A timestamp. Use this parameter to retrieve the time when the notebook instance was last modified. 
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsLastModifiedTime :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Core.NominalDiffTime)
dnirrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE dnirrsLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | The network interface IDs that Amazon SageMaker created at the time of creating the instance. 
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsNetworkInterfaceId :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Types.NetworkInterfaceId)
dnirrsNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE dnirrsNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | The Amazon Resource Name (ARN) of the notebook instance.
--
-- /Note:/ Consider using 'notebookInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsNotebookInstanceArn :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Types.NotebookInstanceArn)
dnirrsNotebookInstanceArn = Lens.field @"notebookInstanceArn"
{-# INLINEABLE dnirrsNotebookInstanceArn #-}
{-# DEPRECATED notebookInstanceArn "Use generic-lens or generic-optics with 'notebookInstanceArn' instead"  #-}

-- | Returns the name of a notebook instance lifecycle configuration.
--
-- For information about notebook instance lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> 
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsNotebookInstanceLifecycleConfigName :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Types.NotebookInstanceLifecycleConfigName)
dnirrsNotebookInstanceLifecycleConfigName = Lens.field @"notebookInstanceLifecycleConfigName"
{-# INLINEABLE dnirrsNotebookInstanceLifecycleConfigName #-}
{-# DEPRECATED notebookInstanceLifecycleConfigName "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigName' instead"  #-}

-- | The name of the Amazon SageMaker notebook instance. 
--
-- /Note:/ Consider using 'notebookInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsNotebookInstanceName :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Types.NotebookInstanceName)
dnirrsNotebookInstanceName = Lens.field @"notebookInstanceName"
{-# INLINEABLE dnirrsNotebookInstanceName #-}
{-# DEPRECATED notebookInstanceName "Use generic-lens or generic-optics with 'notebookInstanceName' instead"  #-}

-- | The status of the notebook instance.
--
-- /Note:/ Consider using 'notebookInstanceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsNotebookInstanceStatus :: Lens.Lens' DescribeNotebookInstanceResponse Types.NotebookInstanceStatus
dnirrsNotebookInstanceStatus = Lens.field @"notebookInstanceStatus"
{-# INLINEABLE dnirrsNotebookInstanceStatus #-}
{-# DEPRECATED notebookInstanceStatus "Use generic-lens or generic-optics with 'notebookInstanceStatus' instead"  #-}

-- | The Amazon Resource Name (ARN) of the IAM role associated with the instance. 
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsRoleArn :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Types.RoleArn)
dnirrsRoleArn = Lens.field @"roleArn"
{-# INLINEABLE dnirrsRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | Whether root access is enabled or disabled for users of the notebook instance.
--
-- /Note:/ Consider using 'rootAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsRootAccess :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Types.RootAccess)
dnirrsRootAccess = Lens.field @"rootAccess"
{-# INLINEABLE dnirrsRootAccess #-}
{-# DEPRECATED rootAccess "Use generic-lens or generic-optics with 'rootAccess' instead"  #-}

-- | The IDs of the VPC security groups.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsSecurityGroups :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe [Types.SecurityGroupId])
dnirrsSecurityGroups = Lens.field @"securityGroups"
{-# INLINEABLE dnirrsSecurityGroups #-}
{-# DEPRECATED securityGroups "Use generic-lens or generic-optics with 'securityGroups' instead"  #-}

-- | The ID of the VPC subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsSubnetId :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Types.SubnetId)
dnirrsSubnetId = Lens.field @"subnetId"
{-# INLINEABLE dnirrsSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | The URL that you use to connect to the Jupyter notebook that is running in your notebook instance. 
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsUrl :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Types.Url)
dnirrsUrl = Lens.field @"url"
{-# INLINEABLE dnirrsUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

-- | The size, in GB, of the ML storage volume attached to the notebook instance.
--
-- /Note:/ Consider using 'volumeSizeInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsVolumeSizeInGB :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Core.Natural)
dnirrsVolumeSizeInGB = Lens.field @"volumeSizeInGB"
{-# INLINEABLE dnirrsVolumeSizeInGB #-}
{-# DEPRECATED volumeSizeInGB "Use generic-lens or generic-optics with 'volumeSizeInGB' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnirrsResponseStatus :: Lens.Lens' DescribeNotebookInstanceResponse Core.Int
dnirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dnirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

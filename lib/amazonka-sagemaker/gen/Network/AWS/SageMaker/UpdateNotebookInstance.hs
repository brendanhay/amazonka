{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateNotebookInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a notebook instance. NotebookInstance updates include upgrading or downgrading the ML compute instance used for your notebook instance to accommodate changes in your workload requirements.
module Network.AWS.SageMaker.UpdateNotebookInstance
  ( -- * Creating a request
    UpdateNotebookInstance (..),
    mkUpdateNotebookInstance,

    -- ** Request lenses
    uniNotebookInstanceName,
    uniAcceleratorTypes,
    uniAdditionalCodeRepositories,
    uniDefaultCodeRepository,
    uniDisassociateAcceleratorTypes,
    uniDisassociateAdditionalCodeRepositories,
    uniDisassociateDefaultCodeRepository,
    uniDisassociateLifecycleConfig,
    uniInstanceType,
    uniLifecycleConfigName,
    uniRoleArn,
    uniRootAccess,
    uniVolumeSizeInGB,

    -- * Destructuring the response
    UpdateNotebookInstanceResponse (..),
    mkUpdateNotebookInstanceResponse,

    -- ** Response lenses
    unirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkUpdateNotebookInstance' smart constructor.
data UpdateNotebookInstance = UpdateNotebookInstance'
  { -- | The name of the notebook instance to update.
    notebookInstanceName :: Types.NotebookInstanceName,
    -- | A list of the Elastic Inference (EI) instance types to associate with this notebook instance. Currently only one EI instance type can be associated with a notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
    acceleratorTypes :: Core.Maybe [Types.NotebookInstanceAcceleratorType],
    -- | An array of up to three Git repositories to associate with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
    additionalCodeRepositories :: Core.Maybe [Types.CodeRepositoryNameOrUrl],
    -- | The Git repository to associate with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
    defaultCodeRepository :: Core.Maybe Types.DefaultCodeRepository,
    -- | A list of the Elastic Inference (EI) instance types to remove from this notebook instance. This operation is idempotent. If you specify an accelerator type that is not associated with the notebook instance when you call this method, it does not throw an error.
    disassociateAcceleratorTypes :: Core.Maybe Core.Bool,
    -- | A list of names or URLs of the default Git repositories to remove from this notebook instance. This operation is idempotent. If you specify a Git repository that is not associated with the notebook instance when you call this method, it does not throw an error.
    disassociateAdditionalCodeRepositories :: Core.Maybe Core.Bool,
    -- | The name or URL of the default Git repository to remove from this notebook instance. This operation is idempotent. If you specify a Git repository that is not associated with the notebook instance when you call this method, it does not throw an error.
    disassociateDefaultCodeRepository :: Core.Maybe Core.Bool,
    -- | Set to @true@ to remove the notebook instance lifecycle configuration currently associated with the notebook instance. This operation is idempotent. If you specify a lifecycle configuration that is not associated with the notebook instance when you call this method, it does not throw an error.
    disassociateLifecycleConfig :: Core.Maybe Core.Bool,
    -- | The Amazon ML compute instance type.
    instanceType :: Core.Maybe Types.InstanceType,
    -- | The name of a lifecycle configuration to associate with the notebook instance. For information about lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
    lifecycleConfigName :: Core.Maybe Types.LifecycleConfigName,
    -- | The Amazon Resource Name (ARN) of the IAM role that Amazon SageMaker can assume to access the notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
    roleArn :: Core.Maybe Types.RoleArn,
    -- | Whether root access is enabled or disabled for users of the notebook instance. The default value is @Enabled@ .
    rootAccess :: Core.Maybe Types.RootAccess,
    -- | The size, in GB, of the ML storage volume to attach to the notebook instance. The default value is 5 GB. ML storage volumes are encrypted, so Amazon SageMaker can't determine the amount of available free space on the volume. Because of this, you can increase the volume size when you update a notebook instance, but you can't decrease the volume size. If you want to decrease the size of the ML storage volume in use, create a new notebook instance with the desired size.
    volumeSizeInGB :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateNotebookInstance' value with any optional fields omitted.
mkUpdateNotebookInstance ::
  -- | 'notebookInstanceName'
  Types.NotebookInstanceName ->
  UpdateNotebookInstance
mkUpdateNotebookInstance notebookInstanceName =
  UpdateNotebookInstance'
    { notebookInstanceName,
      acceleratorTypes = Core.Nothing,
      additionalCodeRepositories = Core.Nothing,
      defaultCodeRepository = Core.Nothing,
      disassociateAcceleratorTypes = Core.Nothing,
      disassociateAdditionalCodeRepositories = Core.Nothing,
      disassociateDefaultCodeRepository = Core.Nothing,
      disassociateLifecycleConfig = Core.Nothing,
      instanceType = Core.Nothing,
      lifecycleConfigName = Core.Nothing,
      roleArn = Core.Nothing,
      rootAccess = Core.Nothing,
      volumeSizeInGB = Core.Nothing
    }

-- | The name of the notebook instance to update.
--
-- /Note:/ Consider using 'notebookInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniNotebookInstanceName :: Lens.Lens' UpdateNotebookInstance Types.NotebookInstanceName
uniNotebookInstanceName = Lens.field @"notebookInstanceName"
{-# DEPRECATED uniNotebookInstanceName "Use generic-lens or generic-optics with 'notebookInstanceName' instead." #-}

-- | A list of the Elastic Inference (EI) instance types to associate with this notebook instance. Currently only one EI instance type can be associated with a notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
--
-- /Note:/ Consider using 'acceleratorTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniAcceleratorTypes :: Lens.Lens' UpdateNotebookInstance (Core.Maybe [Types.NotebookInstanceAcceleratorType])
uniAcceleratorTypes = Lens.field @"acceleratorTypes"
{-# DEPRECATED uniAcceleratorTypes "Use generic-lens or generic-optics with 'acceleratorTypes' instead." #-}

-- | An array of up to three Git repositories to associate with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
--
-- /Note:/ Consider using 'additionalCodeRepositories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniAdditionalCodeRepositories :: Lens.Lens' UpdateNotebookInstance (Core.Maybe [Types.CodeRepositoryNameOrUrl])
uniAdditionalCodeRepositories = Lens.field @"additionalCodeRepositories"
{-# DEPRECATED uniAdditionalCodeRepositories "Use generic-lens or generic-optics with 'additionalCodeRepositories' instead." #-}

-- | The Git repository to associate with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
--
-- /Note:/ Consider using 'defaultCodeRepository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniDefaultCodeRepository :: Lens.Lens' UpdateNotebookInstance (Core.Maybe Types.DefaultCodeRepository)
uniDefaultCodeRepository = Lens.field @"defaultCodeRepository"
{-# DEPRECATED uniDefaultCodeRepository "Use generic-lens or generic-optics with 'defaultCodeRepository' instead." #-}

-- | A list of the Elastic Inference (EI) instance types to remove from this notebook instance. This operation is idempotent. If you specify an accelerator type that is not associated with the notebook instance when you call this method, it does not throw an error.
--
-- /Note:/ Consider using 'disassociateAcceleratorTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniDisassociateAcceleratorTypes :: Lens.Lens' UpdateNotebookInstance (Core.Maybe Core.Bool)
uniDisassociateAcceleratorTypes = Lens.field @"disassociateAcceleratorTypes"
{-# DEPRECATED uniDisassociateAcceleratorTypes "Use generic-lens or generic-optics with 'disassociateAcceleratorTypes' instead." #-}

-- | A list of names or URLs of the default Git repositories to remove from this notebook instance. This operation is idempotent. If you specify a Git repository that is not associated with the notebook instance when you call this method, it does not throw an error.
--
-- /Note:/ Consider using 'disassociateAdditionalCodeRepositories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniDisassociateAdditionalCodeRepositories :: Lens.Lens' UpdateNotebookInstance (Core.Maybe Core.Bool)
uniDisassociateAdditionalCodeRepositories = Lens.field @"disassociateAdditionalCodeRepositories"
{-# DEPRECATED uniDisassociateAdditionalCodeRepositories "Use generic-lens or generic-optics with 'disassociateAdditionalCodeRepositories' instead." #-}

-- | The name or URL of the default Git repository to remove from this notebook instance. This operation is idempotent. If you specify a Git repository that is not associated with the notebook instance when you call this method, it does not throw an error.
--
-- /Note:/ Consider using 'disassociateDefaultCodeRepository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniDisassociateDefaultCodeRepository :: Lens.Lens' UpdateNotebookInstance (Core.Maybe Core.Bool)
uniDisassociateDefaultCodeRepository = Lens.field @"disassociateDefaultCodeRepository"
{-# DEPRECATED uniDisassociateDefaultCodeRepository "Use generic-lens or generic-optics with 'disassociateDefaultCodeRepository' instead." #-}

-- | Set to @true@ to remove the notebook instance lifecycle configuration currently associated with the notebook instance. This operation is idempotent. If you specify a lifecycle configuration that is not associated with the notebook instance when you call this method, it does not throw an error.
--
-- /Note:/ Consider using 'disassociateLifecycleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniDisassociateLifecycleConfig :: Lens.Lens' UpdateNotebookInstance (Core.Maybe Core.Bool)
uniDisassociateLifecycleConfig = Lens.field @"disassociateLifecycleConfig"
{-# DEPRECATED uniDisassociateLifecycleConfig "Use generic-lens or generic-optics with 'disassociateLifecycleConfig' instead." #-}

-- | The Amazon ML compute instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniInstanceType :: Lens.Lens' UpdateNotebookInstance (Core.Maybe Types.InstanceType)
uniInstanceType = Lens.field @"instanceType"
{-# DEPRECATED uniInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The name of a lifecycle configuration to associate with the notebook instance. For information about lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
--
-- /Note:/ Consider using 'lifecycleConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniLifecycleConfigName :: Lens.Lens' UpdateNotebookInstance (Core.Maybe Types.LifecycleConfigName)
uniLifecycleConfigName = Lens.field @"lifecycleConfigName"
{-# DEPRECATED uniLifecycleConfigName "Use generic-lens or generic-optics with 'lifecycleConfigName' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role that Amazon SageMaker can assume to access the notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniRoleArn :: Lens.Lens' UpdateNotebookInstance (Core.Maybe Types.RoleArn)
uniRoleArn = Lens.field @"roleArn"
{-# DEPRECATED uniRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | Whether root access is enabled or disabled for users of the notebook instance. The default value is @Enabled@ .
--
-- /Note:/ Consider using 'rootAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniRootAccess :: Lens.Lens' UpdateNotebookInstance (Core.Maybe Types.RootAccess)
uniRootAccess = Lens.field @"rootAccess"
{-# DEPRECATED uniRootAccess "Use generic-lens or generic-optics with 'rootAccess' instead." #-}

-- | The size, in GB, of the ML storage volume to attach to the notebook instance. The default value is 5 GB. ML storage volumes are encrypted, so Amazon SageMaker can't determine the amount of available free space on the volume. Because of this, you can increase the volume size when you update a notebook instance, but you can't decrease the volume size. If you want to decrease the size of the ML storage volume in use, create a new notebook instance with the desired size.
--
-- /Note:/ Consider using 'volumeSizeInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniVolumeSizeInGB :: Lens.Lens' UpdateNotebookInstance (Core.Maybe Core.Natural)
uniVolumeSizeInGB = Lens.field @"volumeSizeInGB"
{-# DEPRECATED uniVolumeSizeInGB "Use generic-lens or generic-optics with 'volumeSizeInGB' instead." #-}

instance Core.FromJSON UpdateNotebookInstance where
  toJSON UpdateNotebookInstance {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("NotebookInstanceName" Core..= notebookInstanceName),
            ("AcceleratorTypes" Core..=) Core.<$> acceleratorTypes,
            ("AdditionalCodeRepositories" Core..=)
              Core.<$> additionalCodeRepositories,
            ("DefaultCodeRepository" Core..=) Core.<$> defaultCodeRepository,
            ("DisassociateAcceleratorTypes" Core..=)
              Core.<$> disassociateAcceleratorTypes,
            ("DisassociateAdditionalCodeRepositories" Core..=)
              Core.<$> disassociateAdditionalCodeRepositories,
            ("DisassociateDefaultCodeRepository" Core..=)
              Core.<$> disassociateDefaultCodeRepository,
            ("DisassociateLifecycleConfig" Core..=)
              Core.<$> disassociateLifecycleConfig,
            ("InstanceType" Core..=) Core.<$> instanceType,
            ("LifecycleConfigName" Core..=) Core.<$> lifecycleConfigName,
            ("RoleArn" Core..=) Core.<$> roleArn,
            ("RootAccess" Core..=) Core.<$> rootAccess,
            ("VolumeSizeInGB" Core..=) Core.<$> volumeSizeInGB
          ]
      )

instance Core.AWSRequest UpdateNotebookInstance where
  type Rs UpdateNotebookInstance = UpdateNotebookInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.UpdateNotebookInstance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateNotebookInstanceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateNotebookInstanceResponse' smart constructor.
newtype UpdateNotebookInstanceResponse = UpdateNotebookInstanceResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateNotebookInstanceResponse' value with any optional fields omitted.
mkUpdateNotebookInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateNotebookInstanceResponse
mkUpdateNotebookInstanceResponse responseStatus =
  UpdateNotebookInstanceResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unirrsResponseStatus :: Lens.Lens' UpdateNotebookInstanceResponse Core.Int
unirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED unirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

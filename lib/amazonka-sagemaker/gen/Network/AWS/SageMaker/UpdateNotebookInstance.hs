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
    uniAcceleratorTypes,
    uniNotebookInstanceName,
    uniDisassociateAdditionalCodeRepositories,
    uniAdditionalCodeRepositories,
    uniLifecycleConfigName,
    uniDisassociateLifecycleConfig,
    uniDisassociateDefaultCodeRepository,
    uniInstanceType,
    uniDefaultCodeRepository,
    uniVolumeSizeInGB,
    uniRootAccess,
    uniDisassociateAcceleratorTypes,
    uniRoleARN,

    -- * Destructuring the response
    UpdateNotebookInstanceResponse (..),
    mkUpdateNotebookInstanceResponse,

    -- ** Response lenses
    unirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkUpdateNotebookInstance' smart constructor.
data UpdateNotebookInstance = UpdateNotebookInstance'
  { -- | A list of the Elastic Inference (EI) instance types to associate with this notebook instance. Currently only one EI instance type can be associated with a notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
    acceleratorTypes :: Lude.Maybe [NotebookInstanceAcceleratorType],
    -- | The name of the notebook instance to update.
    notebookInstanceName :: Lude.Text,
    -- | A list of names or URLs of the default Git repositories to remove from this notebook instance. This operation is idempotent. If you specify a Git repository that is not associated with the notebook instance when you call this method, it does not throw an error.
    disassociateAdditionalCodeRepositories :: Lude.Maybe Lude.Bool,
    -- | An array of up to three Git repositories to associate with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
    additionalCodeRepositories :: Lude.Maybe [Lude.Text],
    -- | The name of a lifecycle configuration to associate with the notebook instance. For information about lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
    lifecycleConfigName :: Lude.Maybe Lude.Text,
    -- | Set to @true@ to remove the notebook instance lifecycle configuration currently associated with the notebook instance. This operation is idempotent. If you specify a lifecycle configuration that is not associated with the notebook instance when you call this method, it does not throw an error.
    disassociateLifecycleConfig :: Lude.Maybe Lude.Bool,
    -- | The name or URL of the default Git repository to remove from this notebook instance. This operation is idempotent. If you specify a Git repository that is not associated with the notebook instance when you call this method, it does not throw an error.
    disassociateDefaultCodeRepository :: Lude.Maybe Lude.Bool,
    -- | The Amazon ML compute instance type.
    instanceType :: Lude.Maybe InstanceType,
    -- | The Git repository to associate with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
    defaultCodeRepository :: Lude.Maybe Lude.Text,
    -- | The size, in GB, of the ML storage volume to attach to the notebook instance. The default value is 5 GB. ML storage volumes are encrypted, so Amazon SageMaker can't determine the amount of available free space on the volume. Because of this, you can increase the volume size when you update a notebook instance, but you can't decrease the volume size. If you want to decrease the size of the ML storage volume in use, create a new notebook instance with the desired size.
    volumeSizeInGB :: Lude.Maybe Lude.Natural,
    -- | Whether root access is enabled or disabled for users of the notebook instance. The default value is @Enabled@ .
    rootAccess :: Lude.Maybe RootAccess,
    -- | A list of the Elastic Inference (EI) instance types to remove from this notebook instance. This operation is idempotent. If you specify an accelerator type that is not associated with the notebook instance when you call this method, it does not throw an error.
    disassociateAcceleratorTypes :: Lude.Maybe Lude.Bool,
    -- | The Amazon Resource Name (ARN) of the IAM role that Amazon SageMaker can assume to access the notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateNotebookInstance' with the minimum fields required to make a request.
--
-- * 'acceleratorTypes' - A list of the Elastic Inference (EI) instance types to associate with this notebook instance. Currently only one EI instance type can be associated with a notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
-- * 'notebookInstanceName' - The name of the notebook instance to update.
-- * 'disassociateAdditionalCodeRepositories' - A list of names or URLs of the default Git repositories to remove from this notebook instance. This operation is idempotent. If you specify a Git repository that is not associated with the notebook instance when you call this method, it does not throw an error.
-- * 'additionalCodeRepositories' - An array of up to three Git repositories to associate with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
-- * 'lifecycleConfigName' - The name of a lifecycle configuration to associate with the notebook instance. For information about lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
-- * 'disassociateLifecycleConfig' - Set to @true@ to remove the notebook instance lifecycle configuration currently associated with the notebook instance. This operation is idempotent. If you specify a lifecycle configuration that is not associated with the notebook instance when you call this method, it does not throw an error.
-- * 'disassociateDefaultCodeRepository' - The name or URL of the default Git repository to remove from this notebook instance. This operation is idempotent. If you specify a Git repository that is not associated with the notebook instance when you call this method, it does not throw an error.
-- * 'instanceType' - The Amazon ML compute instance type.
-- * 'defaultCodeRepository' - The Git repository to associate with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
-- * 'volumeSizeInGB' - The size, in GB, of the ML storage volume to attach to the notebook instance. The default value is 5 GB. ML storage volumes are encrypted, so Amazon SageMaker can't determine the amount of available free space on the volume. Because of this, you can increase the volume size when you update a notebook instance, but you can't decrease the volume size. If you want to decrease the size of the ML storage volume in use, create a new notebook instance with the desired size.
-- * 'rootAccess' - Whether root access is enabled or disabled for users of the notebook instance. The default value is @Enabled@ .
-- * 'disassociateAcceleratorTypes' - A list of the Elastic Inference (EI) instance types to remove from this notebook instance. This operation is idempotent. If you specify an accelerator type that is not associated with the notebook instance when you call this method, it does not throw an error.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the IAM role that Amazon SageMaker can assume to access the notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
mkUpdateNotebookInstance ::
  -- | 'notebookInstanceName'
  Lude.Text ->
  UpdateNotebookInstance
mkUpdateNotebookInstance pNotebookInstanceName_ =
  UpdateNotebookInstance'
    { acceleratorTypes = Lude.Nothing,
      notebookInstanceName = pNotebookInstanceName_,
      disassociateAdditionalCodeRepositories = Lude.Nothing,
      additionalCodeRepositories = Lude.Nothing,
      lifecycleConfigName = Lude.Nothing,
      disassociateLifecycleConfig = Lude.Nothing,
      disassociateDefaultCodeRepository = Lude.Nothing,
      instanceType = Lude.Nothing,
      defaultCodeRepository = Lude.Nothing,
      volumeSizeInGB = Lude.Nothing,
      rootAccess = Lude.Nothing,
      disassociateAcceleratorTypes = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | A list of the Elastic Inference (EI) instance types to associate with this notebook instance. Currently only one EI instance type can be associated with a notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
--
-- /Note:/ Consider using 'acceleratorTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniAcceleratorTypes :: Lens.Lens' UpdateNotebookInstance (Lude.Maybe [NotebookInstanceAcceleratorType])
uniAcceleratorTypes = Lens.lens (acceleratorTypes :: UpdateNotebookInstance -> Lude.Maybe [NotebookInstanceAcceleratorType]) (\s a -> s {acceleratorTypes = a} :: UpdateNotebookInstance)
{-# DEPRECATED uniAcceleratorTypes "Use generic-lens or generic-optics with 'acceleratorTypes' instead." #-}

-- | The name of the notebook instance to update.
--
-- /Note:/ Consider using 'notebookInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniNotebookInstanceName :: Lens.Lens' UpdateNotebookInstance Lude.Text
uniNotebookInstanceName = Lens.lens (notebookInstanceName :: UpdateNotebookInstance -> Lude.Text) (\s a -> s {notebookInstanceName = a} :: UpdateNotebookInstance)
{-# DEPRECATED uniNotebookInstanceName "Use generic-lens or generic-optics with 'notebookInstanceName' instead." #-}

-- | A list of names or URLs of the default Git repositories to remove from this notebook instance. This operation is idempotent. If you specify a Git repository that is not associated with the notebook instance when you call this method, it does not throw an error.
--
-- /Note:/ Consider using 'disassociateAdditionalCodeRepositories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniDisassociateAdditionalCodeRepositories :: Lens.Lens' UpdateNotebookInstance (Lude.Maybe Lude.Bool)
uniDisassociateAdditionalCodeRepositories = Lens.lens (disassociateAdditionalCodeRepositories :: UpdateNotebookInstance -> Lude.Maybe Lude.Bool) (\s a -> s {disassociateAdditionalCodeRepositories = a} :: UpdateNotebookInstance)
{-# DEPRECATED uniDisassociateAdditionalCodeRepositories "Use generic-lens or generic-optics with 'disassociateAdditionalCodeRepositories' instead." #-}

-- | An array of up to three Git repositories to associate with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
--
-- /Note:/ Consider using 'additionalCodeRepositories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniAdditionalCodeRepositories :: Lens.Lens' UpdateNotebookInstance (Lude.Maybe [Lude.Text])
uniAdditionalCodeRepositories = Lens.lens (additionalCodeRepositories :: UpdateNotebookInstance -> Lude.Maybe [Lude.Text]) (\s a -> s {additionalCodeRepositories = a} :: UpdateNotebookInstance)
{-# DEPRECATED uniAdditionalCodeRepositories "Use generic-lens or generic-optics with 'additionalCodeRepositories' instead." #-}

-- | The name of a lifecycle configuration to associate with the notebook instance. For information about lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
--
-- /Note:/ Consider using 'lifecycleConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniLifecycleConfigName :: Lens.Lens' UpdateNotebookInstance (Lude.Maybe Lude.Text)
uniLifecycleConfigName = Lens.lens (lifecycleConfigName :: UpdateNotebookInstance -> Lude.Maybe Lude.Text) (\s a -> s {lifecycleConfigName = a} :: UpdateNotebookInstance)
{-# DEPRECATED uniLifecycleConfigName "Use generic-lens or generic-optics with 'lifecycleConfigName' instead." #-}

-- | Set to @true@ to remove the notebook instance lifecycle configuration currently associated with the notebook instance. This operation is idempotent. If you specify a lifecycle configuration that is not associated with the notebook instance when you call this method, it does not throw an error.
--
-- /Note:/ Consider using 'disassociateLifecycleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniDisassociateLifecycleConfig :: Lens.Lens' UpdateNotebookInstance (Lude.Maybe Lude.Bool)
uniDisassociateLifecycleConfig = Lens.lens (disassociateLifecycleConfig :: UpdateNotebookInstance -> Lude.Maybe Lude.Bool) (\s a -> s {disassociateLifecycleConfig = a} :: UpdateNotebookInstance)
{-# DEPRECATED uniDisassociateLifecycleConfig "Use generic-lens or generic-optics with 'disassociateLifecycleConfig' instead." #-}

-- | The name or URL of the default Git repository to remove from this notebook instance. This operation is idempotent. If you specify a Git repository that is not associated with the notebook instance when you call this method, it does not throw an error.
--
-- /Note:/ Consider using 'disassociateDefaultCodeRepository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniDisassociateDefaultCodeRepository :: Lens.Lens' UpdateNotebookInstance (Lude.Maybe Lude.Bool)
uniDisassociateDefaultCodeRepository = Lens.lens (disassociateDefaultCodeRepository :: UpdateNotebookInstance -> Lude.Maybe Lude.Bool) (\s a -> s {disassociateDefaultCodeRepository = a} :: UpdateNotebookInstance)
{-# DEPRECATED uniDisassociateDefaultCodeRepository "Use generic-lens or generic-optics with 'disassociateDefaultCodeRepository' instead." #-}

-- | The Amazon ML compute instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniInstanceType :: Lens.Lens' UpdateNotebookInstance (Lude.Maybe InstanceType)
uniInstanceType = Lens.lens (instanceType :: UpdateNotebookInstance -> Lude.Maybe InstanceType) (\s a -> s {instanceType = a} :: UpdateNotebookInstance)
{-# DEPRECATED uniInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The Git repository to associate with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
--
-- /Note:/ Consider using 'defaultCodeRepository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniDefaultCodeRepository :: Lens.Lens' UpdateNotebookInstance (Lude.Maybe Lude.Text)
uniDefaultCodeRepository = Lens.lens (defaultCodeRepository :: UpdateNotebookInstance -> Lude.Maybe Lude.Text) (\s a -> s {defaultCodeRepository = a} :: UpdateNotebookInstance)
{-# DEPRECATED uniDefaultCodeRepository "Use generic-lens or generic-optics with 'defaultCodeRepository' instead." #-}

-- | The size, in GB, of the ML storage volume to attach to the notebook instance. The default value is 5 GB. ML storage volumes are encrypted, so Amazon SageMaker can't determine the amount of available free space on the volume. Because of this, you can increase the volume size when you update a notebook instance, but you can't decrease the volume size. If you want to decrease the size of the ML storage volume in use, create a new notebook instance with the desired size.
--
-- /Note:/ Consider using 'volumeSizeInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniVolumeSizeInGB :: Lens.Lens' UpdateNotebookInstance (Lude.Maybe Lude.Natural)
uniVolumeSizeInGB = Lens.lens (volumeSizeInGB :: UpdateNotebookInstance -> Lude.Maybe Lude.Natural) (\s a -> s {volumeSizeInGB = a} :: UpdateNotebookInstance)
{-# DEPRECATED uniVolumeSizeInGB "Use generic-lens or generic-optics with 'volumeSizeInGB' instead." #-}

-- | Whether root access is enabled or disabled for users of the notebook instance. The default value is @Enabled@ .
--
-- /Note:/ Consider using 'rootAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniRootAccess :: Lens.Lens' UpdateNotebookInstance (Lude.Maybe RootAccess)
uniRootAccess = Lens.lens (rootAccess :: UpdateNotebookInstance -> Lude.Maybe RootAccess) (\s a -> s {rootAccess = a} :: UpdateNotebookInstance)
{-# DEPRECATED uniRootAccess "Use generic-lens or generic-optics with 'rootAccess' instead." #-}

-- | A list of the Elastic Inference (EI) instance types to remove from this notebook instance. This operation is idempotent. If you specify an accelerator type that is not associated with the notebook instance when you call this method, it does not throw an error.
--
-- /Note:/ Consider using 'disassociateAcceleratorTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniDisassociateAcceleratorTypes :: Lens.Lens' UpdateNotebookInstance (Lude.Maybe Lude.Bool)
uniDisassociateAcceleratorTypes = Lens.lens (disassociateAcceleratorTypes :: UpdateNotebookInstance -> Lude.Maybe Lude.Bool) (\s a -> s {disassociateAcceleratorTypes = a} :: UpdateNotebookInstance)
{-# DEPRECATED uniDisassociateAcceleratorTypes "Use generic-lens or generic-optics with 'disassociateAcceleratorTypes' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role that Amazon SageMaker can assume to access the notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uniRoleARN :: Lens.Lens' UpdateNotebookInstance (Lude.Maybe Lude.Text)
uniRoleARN = Lens.lens (roleARN :: UpdateNotebookInstance -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: UpdateNotebookInstance)
{-# DEPRECATED uniRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest UpdateNotebookInstance where
  type Rs UpdateNotebookInstance = UpdateNotebookInstanceResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateNotebookInstanceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateNotebookInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.UpdateNotebookInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateNotebookInstance where
  toJSON UpdateNotebookInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceleratorTypes" Lude..=) Lude.<$> acceleratorTypes,
            Lude.Just ("NotebookInstanceName" Lude..= notebookInstanceName),
            ("DisassociateAdditionalCodeRepositories" Lude..=)
              Lude.<$> disassociateAdditionalCodeRepositories,
            ("AdditionalCodeRepositories" Lude..=)
              Lude.<$> additionalCodeRepositories,
            ("LifecycleConfigName" Lude..=) Lude.<$> lifecycleConfigName,
            ("DisassociateLifecycleConfig" Lude..=)
              Lude.<$> disassociateLifecycleConfig,
            ("DisassociateDefaultCodeRepository" Lude..=)
              Lude.<$> disassociateDefaultCodeRepository,
            ("InstanceType" Lude..=) Lude.<$> instanceType,
            ("DefaultCodeRepository" Lude..=) Lude.<$> defaultCodeRepository,
            ("VolumeSizeInGB" Lude..=) Lude.<$> volumeSizeInGB,
            ("RootAccess" Lude..=) Lude.<$> rootAccess,
            ("DisassociateAcceleratorTypes" Lude..=)
              Lude.<$> disassociateAcceleratorTypes,
            ("RoleArn" Lude..=) Lude.<$> roleARN
          ]
      )

instance Lude.ToPath UpdateNotebookInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateNotebookInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateNotebookInstanceResponse' smart constructor.
newtype UpdateNotebookInstanceResponse = UpdateNotebookInstanceResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateNotebookInstanceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateNotebookInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateNotebookInstanceResponse
mkUpdateNotebookInstanceResponse pResponseStatus_ =
  UpdateNotebookInstanceResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unirsResponseStatus :: Lens.Lens' UpdateNotebookInstanceResponse Lude.Int
unirsResponseStatus = Lens.lens (responseStatus :: UpdateNotebookInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateNotebookInstanceResponse)
{-# DEPRECATED unirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.RegisterJobDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an AWS Batch job definition.
module Network.AWS.Batch.RegisterJobDefinition
  ( -- * Creating a request
    RegisterJobDefinition (..),
    mkRegisterJobDefinition,

    -- ** Request lenses
    rjdJobDefinitionName,
    rjdRetryStrategy,
    rjdParameters,
    rjdType,
    rjdTimeout,
    rjdContainerProperties,
    rjdNodeProperties,
    rjdTags,

    -- * Destructuring the response
    RegisterJobDefinitionResponse (..),
    mkRegisterJobDefinitionResponse,

    -- ** Response lenses
    rjdrsJobDefinitionName,
    rjdrsJobDefinitionARN,
    rjdrsRevision,
    rjdrsResponseStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterJobDefinition' smart constructor.
data RegisterJobDefinition = RegisterJobDefinition'
  { -- | The name of the job definition to register. Up to 128 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
    jobDefinitionName :: Lude.Text,
    -- | The retry strategy to use for failed jobs that are submitted with this job definition. Any retry strategy that is specified during a 'SubmitJob' operation overrides the retry strategy defined here. If a job is terminated due to a timeout, it is not retried.
    retryStrategy :: Lude.Maybe RetryStrategy,
    -- | Default parameter substitution placeholders to set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition.
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The type of job definition.
    type' :: JobDefinitionType,
    -- | The timeout configuration for jobs that are submitted with this job definition, after which AWS Batch terminates your jobs if they have not finished. If a job is terminated due to a timeout, it is not retried. The minimum value for the timeout is 60 seconds. Any timeout configuration that is specified during a 'SubmitJob' operation overrides the timeout configuration defined here. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/job_timeouts.html Job Timeouts> in the /Amazon Elastic Container Service Developer Guide/ .
    timeout :: Lude.Maybe JobTimeout,
    -- | An object with various properties specific to single-node container-based jobs. If the job definition's @type@ parameter is @container@ , then you must specify either @containerProperties@ or @nodeProperties@ .
    containerProperties :: Lude.Maybe ContainerProperties,
    -- | An object with various properties specific to multi-node parallel jobs. If you specify node properties for a job, it becomes a multi-node parallel job. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-parallel-jobs.html Multi-node Parallel Jobs> in the /AWS Batch User Guide/ . If the job definition's @type@ parameter is @container@ , then you must specify either @containerProperties@ or @nodeProperties@ .
    nodeProperties :: Lude.Maybe NodeProperties,
    -- | The tags that you apply to the job definition to help you categorize and organize your resources. Each tag consists of a key and an optional value. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in /AWS General Reference/ .
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterJobDefinition' with the minimum fields required to make a request.
--
-- * 'jobDefinitionName' - The name of the job definition to register. Up to 128 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
-- * 'retryStrategy' - The retry strategy to use for failed jobs that are submitted with this job definition. Any retry strategy that is specified during a 'SubmitJob' operation overrides the retry strategy defined here. If a job is terminated due to a timeout, it is not retried.
-- * 'parameters' - Default parameter substitution placeholders to set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition.
-- * 'type'' - The type of job definition.
-- * 'timeout' - The timeout configuration for jobs that are submitted with this job definition, after which AWS Batch terminates your jobs if they have not finished. If a job is terminated due to a timeout, it is not retried. The minimum value for the timeout is 60 seconds. Any timeout configuration that is specified during a 'SubmitJob' operation overrides the timeout configuration defined here. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/job_timeouts.html Job Timeouts> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'containerProperties' - An object with various properties specific to single-node container-based jobs. If the job definition's @type@ parameter is @container@ , then you must specify either @containerProperties@ or @nodeProperties@ .
-- * 'nodeProperties' - An object with various properties specific to multi-node parallel jobs. If you specify node properties for a job, it becomes a multi-node parallel job. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-parallel-jobs.html Multi-node Parallel Jobs> in the /AWS Batch User Guide/ . If the job definition's @type@ parameter is @container@ , then you must specify either @containerProperties@ or @nodeProperties@ .
-- * 'tags' - The tags that you apply to the job definition to help you categorize and organize your resources. Each tag consists of a key and an optional value. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in /AWS General Reference/ .
mkRegisterJobDefinition ::
  -- | 'jobDefinitionName'
  Lude.Text ->
  -- | 'type''
  JobDefinitionType ->
  RegisterJobDefinition
mkRegisterJobDefinition pJobDefinitionName_ pType_ =
  RegisterJobDefinition'
    { jobDefinitionName = pJobDefinitionName_,
      retryStrategy = Lude.Nothing,
      parameters = Lude.Nothing,
      type' = pType_,
      timeout = Lude.Nothing,
      containerProperties = Lude.Nothing,
      nodeProperties = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The name of the job definition to register. Up to 128 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
--
-- /Note:/ Consider using 'jobDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdJobDefinitionName :: Lens.Lens' RegisterJobDefinition Lude.Text
rjdJobDefinitionName = Lens.lens (jobDefinitionName :: RegisterJobDefinition -> Lude.Text) (\s a -> s {jobDefinitionName = a} :: RegisterJobDefinition)
{-# DEPRECATED rjdJobDefinitionName "Use generic-lens or generic-optics with 'jobDefinitionName' instead." #-}

-- | The retry strategy to use for failed jobs that are submitted with this job definition. Any retry strategy that is specified during a 'SubmitJob' operation overrides the retry strategy defined here. If a job is terminated due to a timeout, it is not retried.
--
-- /Note:/ Consider using 'retryStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdRetryStrategy :: Lens.Lens' RegisterJobDefinition (Lude.Maybe RetryStrategy)
rjdRetryStrategy = Lens.lens (retryStrategy :: RegisterJobDefinition -> Lude.Maybe RetryStrategy) (\s a -> s {retryStrategy = a} :: RegisterJobDefinition)
{-# DEPRECATED rjdRetryStrategy "Use generic-lens or generic-optics with 'retryStrategy' instead." #-}

-- | Default parameter substitution placeholders to set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdParameters :: Lens.Lens' RegisterJobDefinition (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
rjdParameters = Lens.lens (parameters :: RegisterJobDefinition -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: RegisterJobDefinition)
{-# DEPRECATED rjdParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The type of job definition.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdType :: Lens.Lens' RegisterJobDefinition JobDefinitionType
rjdType = Lens.lens (type' :: RegisterJobDefinition -> JobDefinitionType) (\s a -> s {type' = a} :: RegisterJobDefinition)
{-# DEPRECATED rjdType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The timeout configuration for jobs that are submitted with this job definition, after which AWS Batch terminates your jobs if they have not finished. If a job is terminated due to a timeout, it is not retried. The minimum value for the timeout is 60 seconds. Any timeout configuration that is specified during a 'SubmitJob' operation overrides the timeout configuration defined here. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/job_timeouts.html Job Timeouts> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdTimeout :: Lens.Lens' RegisterJobDefinition (Lude.Maybe JobTimeout)
rjdTimeout = Lens.lens (timeout :: RegisterJobDefinition -> Lude.Maybe JobTimeout) (\s a -> s {timeout = a} :: RegisterJobDefinition)
{-# DEPRECATED rjdTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

-- | An object with various properties specific to single-node container-based jobs. If the job definition's @type@ parameter is @container@ , then you must specify either @containerProperties@ or @nodeProperties@ .
--
-- /Note:/ Consider using 'containerProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdContainerProperties :: Lens.Lens' RegisterJobDefinition (Lude.Maybe ContainerProperties)
rjdContainerProperties = Lens.lens (containerProperties :: RegisterJobDefinition -> Lude.Maybe ContainerProperties) (\s a -> s {containerProperties = a} :: RegisterJobDefinition)
{-# DEPRECATED rjdContainerProperties "Use generic-lens or generic-optics with 'containerProperties' instead." #-}

-- | An object with various properties specific to multi-node parallel jobs. If you specify node properties for a job, it becomes a multi-node parallel job. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/multi-node-parallel-jobs.html Multi-node Parallel Jobs> in the /AWS Batch User Guide/ . If the job definition's @type@ parameter is @container@ , then you must specify either @containerProperties@ or @nodeProperties@ .
--
-- /Note:/ Consider using 'nodeProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdNodeProperties :: Lens.Lens' RegisterJobDefinition (Lude.Maybe NodeProperties)
rjdNodeProperties = Lens.lens (nodeProperties :: RegisterJobDefinition -> Lude.Maybe NodeProperties) (\s a -> s {nodeProperties = a} :: RegisterJobDefinition)
{-# DEPRECATED rjdNodeProperties "Use generic-lens or generic-optics with 'nodeProperties' instead." #-}

-- | The tags that you apply to the job definition to help you categorize and organize your resources. Each tag consists of a key and an optional value. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in /AWS General Reference/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdTags :: Lens.Lens' RegisterJobDefinition (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
rjdTags = Lens.lens (tags :: RegisterJobDefinition -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: RegisterJobDefinition)
{-# DEPRECATED rjdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest RegisterJobDefinition where
  type Rs RegisterJobDefinition = RegisterJobDefinitionResponse
  request = Req.postJSON batchService
  response =
    Res.receiveJSON
      ( \s h x ->
          RegisterJobDefinitionResponse'
            Lude.<$> (x Lude..:> "jobDefinitionName")
            Lude.<*> (x Lude..:> "jobDefinitionArn")
            Lude.<*> (x Lude..:> "revision")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterJobDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterJobDefinition where
  toJSON RegisterJobDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("jobDefinitionName" Lude..= jobDefinitionName),
            ("retryStrategy" Lude..=) Lude.<$> retryStrategy,
            ("parameters" Lude..=) Lude.<$> parameters,
            Lude.Just ("type" Lude..= type'),
            ("timeout" Lude..=) Lude.<$> timeout,
            ("containerProperties" Lude..=) Lude.<$> containerProperties,
            ("nodeProperties" Lude..=) Lude.<$> nodeProperties,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath RegisterJobDefinition where
  toPath = Lude.const "/v1/registerjobdefinition"

instance Lude.ToQuery RegisterJobDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterJobDefinitionResponse' smart constructor.
data RegisterJobDefinitionResponse = RegisterJobDefinitionResponse'
  { -- | The name of the job definition.
    jobDefinitionName :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the job definition.
    jobDefinitionARN :: Lude.Text,
    -- | The revision of the job definition.
    revision :: Lude.Int,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterJobDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'jobDefinitionName' - The name of the job definition.
-- * 'jobDefinitionARN' - The Amazon Resource Name (ARN) of the job definition.
-- * 'revision' - The revision of the job definition.
-- * 'responseStatus' - The response status code.
mkRegisterJobDefinitionResponse ::
  -- | 'jobDefinitionName'
  Lude.Text ->
  -- | 'jobDefinitionARN'
  Lude.Text ->
  -- | 'revision'
  Lude.Int ->
  -- | 'responseStatus'
  Lude.Int ->
  RegisterJobDefinitionResponse
mkRegisterJobDefinitionResponse
  pJobDefinitionName_
  pJobDefinitionARN_
  pRevision_
  pResponseStatus_ =
    RegisterJobDefinitionResponse'
      { jobDefinitionName =
          pJobDefinitionName_,
        jobDefinitionARN = pJobDefinitionARN_,
        revision = pRevision_,
        responseStatus = pResponseStatus_
      }

-- | The name of the job definition.
--
-- /Note:/ Consider using 'jobDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdrsJobDefinitionName :: Lens.Lens' RegisterJobDefinitionResponse Lude.Text
rjdrsJobDefinitionName = Lens.lens (jobDefinitionName :: RegisterJobDefinitionResponse -> Lude.Text) (\s a -> s {jobDefinitionName = a} :: RegisterJobDefinitionResponse)
{-# DEPRECATED rjdrsJobDefinitionName "Use generic-lens or generic-optics with 'jobDefinitionName' instead." #-}

-- | The Amazon Resource Name (ARN) of the job definition.
--
-- /Note:/ Consider using 'jobDefinitionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdrsJobDefinitionARN :: Lens.Lens' RegisterJobDefinitionResponse Lude.Text
rjdrsJobDefinitionARN = Lens.lens (jobDefinitionARN :: RegisterJobDefinitionResponse -> Lude.Text) (\s a -> s {jobDefinitionARN = a} :: RegisterJobDefinitionResponse)
{-# DEPRECATED rjdrsJobDefinitionARN "Use generic-lens or generic-optics with 'jobDefinitionARN' instead." #-}

-- | The revision of the job definition.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdrsRevision :: Lens.Lens' RegisterJobDefinitionResponse Lude.Int
rjdrsRevision = Lens.lens (revision :: RegisterJobDefinitionResponse -> Lude.Int) (\s a -> s {revision = a} :: RegisterJobDefinitionResponse)
{-# DEPRECATED rjdrsRevision "Use generic-lens or generic-optics with 'revision' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjdrsResponseStatus :: Lens.Lens' RegisterJobDefinitionResponse Lude.Int
rjdrsResponseStatus = Lens.lens (responseStatus :: RegisterJobDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterJobDefinitionResponse)
{-# DEPRECATED rjdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

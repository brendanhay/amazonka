{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateAutoMLJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Autopilot job.
--
-- Find the best performing model after you run an Autopilot job by calling . Deploy that model by following the steps described in <https://docs.aws.amazon.com/sagemaker/latest/dg/ex1-deploy-model.html Step 6.1: Deploy the Model to Amazon SageMaker Hosting Services> .
-- For information about how to use Autopilot, see <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-automate-model-development.html Automate Model Development with Amazon SageMaker Autopilot> .
module Network.AWS.SageMaker.CreateAutoMLJob
  ( -- * Creating a request
    CreateAutoMLJob (..),
    mkCreateAutoMLJob,

    -- ** Request lenses
    camljGenerateCandidateDefinitionsOnly,
    camljProblemType,
    camljAutoMLJobConfig,
    camljAutoMLJobName,
    camljAutoMLJobObjective,
    camljInputDataConfig,
    camljOutputDataConfig,
    camljTags,
    camljRoleARN,

    -- * Destructuring the response
    CreateAutoMLJobResponse (..),
    mkCreateAutoMLJobResponse,

    -- ** Response lenses
    camljrsAutoMLJobARN,
    camljrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateAutoMLJob' smart constructor.
data CreateAutoMLJob = CreateAutoMLJob'
  { -- | Generates possible candidates without training a model. A candidate is a combination of data preprocessors, algorithms, and algorithm parameter settings.
    generateCandidateDefinitionsOnly :: Lude.Maybe Lude.Bool,
    -- | Defines the kind of preprocessing and algorithms intended for the candidates. Options include: BinaryClassification, MulticlassClassification, and Regression.
    problemType :: Lude.Maybe ProblemType,
    -- | Contains CompletionCriteria and SecurityConfig.
    autoMLJobConfig :: Lude.Maybe AutoMLJobConfig,
    -- | Identifies an Autopilot job. Must be unique to your account and is case-insensitive.
    autoMLJobName :: Lude.Text,
    -- | Defines the objective of a an AutoML job. You provide a 'AutoMLJobObjective$MetricName' and Autopilot infers whether to minimize or maximize it. If a metric is not specified, the most commonly used ObjectiveMetric for problem type is automaically selected.
    autoMLJobObjective :: Lude.Maybe AutoMLJobObjective,
    -- | Similar to InputDataConfig supported by Tuning. Format(s) supported: CSV. Minimum of 500 rows.
    inputDataConfig :: Lude.NonEmpty AutoMLChannel,
    -- | Similar to OutputDataConfig supported by Tuning. Format(s) supported: CSV.
    outputDataConfig :: AutoMLOutputDataConfig,
    -- | Each tag consists of a key and an optional value. Tag keys must be unique per resource.
    tags :: Lude.Maybe [Tag],
    -- | The ARN of the role that is used to access the data.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAutoMLJob' with the minimum fields required to make a request.
--
-- * 'generateCandidateDefinitionsOnly' - Generates possible candidates without training a model. A candidate is a combination of data preprocessors, algorithms, and algorithm parameter settings.
-- * 'problemType' - Defines the kind of preprocessing and algorithms intended for the candidates. Options include: BinaryClassification, MulticlassClassification, and Regression.
-- * 'autoMLJobConfig' - Contains CompletionCriteria and SecurityConfig.
-- * 'autoMLJobName' - Identifies an Autopilot job. Must be unique to your account and is case-insensitive.
-- * 'autoMLJobObjective' - Defines the objective of a an AutoML job. You provide a 'AutoMLJobObjective$MetricName' and Autopilot infers whether to minimize or maximize it. If a metric is not specified, the most commonly used ObjectiveMetric for problem type is automaically selected.
-- * 'inputDataConfig' - Similar to InputDataConfig supported by Tuning. Format(s) supported: CSV. Minimum of 500 rows.
-- * 'outputDataConfig' - Similar to OutputDataConfig supported by Tuning. Format(s) supported: CSV.
-- * 'tags' - Each tag consists of a key and an optional value. Tag keys must be unique per resource.
-- * 'roleARN' - The ARN of the role that is used to access the data.
mkCreateAutoMLJob ::
  -- | 'autoMLJobName'
  Lude.Text ->
  -- | 'inputDataConfig'
  Lude.NonEmpty AutoMLChannel ->
  -- | 'outputDataConfig'
  AutoMLOutputDataConfig ->
  -- | 'roleARN'
  Lude.Text ->
  CreateAutoMLJob
mkCreateAutoMLJob
  pAutoMLJobName_
  pInputDataConfig_
  pOutputDataConfig_
  pRoleARN_ =
    CreateAutoMLJob'
      { generateCandidateDefinitionsOnly = Lude.Nothing,
        problemType = Lude.Nothing,
        autoMLJobConfig = Lude.Nothing,
        autoMLJobName = pAutoMLJobName_,
        autoMLJobObjective = Lude.Nothing,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        tags = Lude.Nothing,
        roleARN = pRoleARN_
      }

-- | Generates possible candidates without training a model. A candidate is a combination of data preprocessors, algorithms, and algorithm parameter settings.
--
-- /Note:/ Consider using 'generateCandidateDefinitionsOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camljGenerateCandidateDefinitionsOnly :: Lens.Lens' CreateAutoMLJob (Lude.Maybe Lude.Bool)
camljGenerateCandidateDefinitionsOnly = Lens.lens (generateCandidateDefinitionsOnly :: CreateAutoMLJob -> Lude.Maybe Lude.Bool) (\s a -> s {generateCandidateDefinitionsOnly = a} :: CreateAutoMLJob)
{-# DEPRECATED camljGenerateCandidateDefinitionsOnly "Use generic-lens or generic-optics with 'generateCandidateDefinitionsOnly' instead." #-}

-- | Defines the kind of preprocessing and algorithms intended for the candidates. Options include: BinaryClassification, MulticlassClassification, and Regression.
--
-- /Note:/ Consider using 'problemType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camljProblemType :: Lens.Lens' CreateAutoMLJob (Lude.Maybe ProblemType)
camljProblemType = Lens.lens (problemType :: CreateAutoMLJob -> Lude.Maybe ProblemType) (\s a -> s {problemType = a} :: CreateAutoMLJob)
{-# DEPRECATED camljProblemType "Use generic-lens or generic-optics with 'problemType' instead." #-}

-- | Contains CompletionCriteria and SecurityConfig.
--
-- /Note:/ Consider using 'autoMLJobConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camljAutoMLJobConfig :: Lens.Lens' CreateAutoMLJob (Lude.Maybe AutoMLJobConfig)
camljAutoMLJobConfig = Lens.lens (autoMLJobConfig :: CreateAutoMLJob -> Lude.Maybe AutoMLJobConfig) (\s a -> s {autoMLJobConfig = a} :: CreateAutoMLJob)
{-# DEPRECATED camljAutoMLJobConfig "Use generic-lens or generic-optics with 'autoMLJobConfig' instead." #-}

-- | Identifies an Autopilot job. Must be unique to your account and is case-insensitive.
--
-- /Note:/ Consider using 'autoMLJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camljAutoMLJobName :: Lens.Lens' CreateAutoMLJob Lude.Text
camljAutoMLJobName = Lens.lens (autoMLJobName :: CreateAutoMLJob -> Lude.Text) (\s a -> s {autoMLJobName = a} :: CreateAutoMLJob)
{-# DEPRECATED camljAutoMLJobName "Use generic-lens or generic-optics with 'autoMLJobName' instead." #-}

-- | Defines the objective of a an AutoML job. You provide a 'AutoMLJobObjective$MetricName' and Autopilot infers whether to minimize or maximize it. If a metric is not specified, the most commonly used ObjectiveMetric for problem type is automaically selected.
--
-- /Note:/ Consider using 'autoMLJobObjective' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camljAutoMLJobObjective :: Lens.Lens' CreateAutoMLJob (Lude.Maybe AutoMLJobObjective)
camljAutoMLJobObjective = Lens.lens (autoMLJobObjective :: CreateAutoMLJob -> Lude.Maybe AutoMLJobObjective) (\s a -> s {autoMLJobObjective = a} :: CreateAutoMLJob)
{-# DEPRECATED camljAutoMLJobObjective "Use generic-lens or generic-optics with 'autoMLJobObjective' instead." #-}

-- | Similar to InputDataConfig supported by Tuning. Format(s) supported: CSV. Minimum of 500 rows.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camljInputDataConfig :: Lens.Lens' CreateAutoMLJob (Lude.NonEmpty AutoMLChannel)
camljInputDataConfig = Lens.lens (inputDataConfig :: CreateAutoMLJob -> Lude.NonEmpty AutoMLChannel) (\s a -> s {inputDataConfig = a} :: CreateAutoMLJob)
{-# DEPRECATED camljInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Similar to OutputDataConfig supported by Tuning. Format(s) supported: CSV.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camljOutputDataConfig :: Lens.Lens' CreateAutoMLJob AutoMLOutputDataConfig
camljOutputDataConfig = Lens.lens (outputDataConfig :: CreateAutoMLJob -> AutoMLOutputDataConfig) (\s a -> s {outputDataConfig = a} :: CreateAutoMLJob)
{-# DEPRECATED camljOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | Each tag consists of a key and an optional value. Tag keys must be unique per resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camljTags :: Lens.Lens' CreateAutoMLJob (Lude.Maybe [Tag])
camljTags = Lens.lens (tags :: CreateAutoMLJob -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateAutoMLJob)
{-# DEPRECATED camljTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ARN of the role that is used to access the data.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camljRoleARN :: Lens.Lens' CreateAutoMLJob Lude.Text
camljRoleARN = Lens.lens (roleARN :: CreateAutoMLJob -> Lude.Text) (\s a -> s {roleARN = a} :: CreateAutoMLJob)
{-# DEPRECATED camljRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest CreateAutoMLJob where
  type Rs CreateAutoMLJob = CreateAutoMLJobResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateAutoMLJobResponse'
            Lude.<$> (x Lude..:> "AutoMLJobArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateAutoMLJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateAutoMLJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateAutoMLJob where
  toJSON CreateAutoMLJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("GenerateCandidateDefinitionsOnly" Lude..=)
              Lude.<$> generateCandidateDefinitionsOnly,
            ("ProblemType" Lude..=) Lude.<$> problemType,
            ("AutoMLJobConfig" Lude..=) Lude.<$> autoMLJobConfig,
            Lude.Just ("AutoMLJobName" Lude..= autoMLJobName),
            ("AutoMLJobObjective" Lude..=) Lude.<$> autoMLJobObjective,
            Lude.Just ("InputDataConfig" Lude..= inputDataConfig),
            Lude.Just ("OutputDataConfig" Lude..= outputDataConfig),
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("RoleArn" Lude..= roleARN)
          ]
      )

instance Lude.ToPath CreateAutoMLJob where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateAutoMLJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateAutoMLJobResponse' smart constructor.
data CreateAutoMLJobResponse = CreateAutoMLJobResponse'
  { -- | When a job is created, it is assigned a unique ARN.
    autoMLJobARN :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAutoMLJobResponse' with the minimum fields required to make a request.
--
-- * 'autoMLJobARN' - When a job is created, it is assigned a unique ARN.
-- * 'responseStatus' - The response status code.
mkCreateAutoMLJobResponse ::
  -- | 'autoMLJobARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateAutoMLJobResponse
mkCreateAutoMLJobResponse pAutoMLJobARN_ pResponseStatus_ =
  CreateAutoMLJobResponse'
    { autoMLJobARN = pAutoMLJobARN_,
      responseStatus = pResponseStatus_
    }

-- | When a job is created, it is assigned a unique ARN.
--
-- /Note:/ Consider using 'autoMLJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camljrsAutoMLJobARN :: Lens.Lens' CreateAutoMLJobResponse Lude.Text
camljrsAutoMLJobARN = Lens.lens (autoMLJobARN :: CreateAutoMLJobResponse -> Lude.Text) (\s a -> s {autoMLJobARN = a} :: CreateAutoMLJobResponse)
{-# DEPRECATED camljrsAutoMLJobARN "Use generic-lens or generic-optics with 'autoMLJobARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camljrsResponseStatus :: Lens.Lens' CreateAutoMLJobResponse Lude.Int
camljrsResponseStatus = Lens.lens (responseStatus :: CreateAutoMLJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAutoMLJobResponse)
{-# DEPRECATED camljrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

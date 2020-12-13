{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.CreateActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an activity. An activity is a task that you write in any programming language and host on any machine that has access to AWS Step Functions. Activities must poll Step Functions using the @GetActivityTask@ API action and respond using @SendTask*@ API actions. This function lets Step Functions know the existence of your activity and returns an identifier for use in a state machine and when polling from the activity.
module Network.AWS.StepFunctions.CreateActivity
  ( -- * Creating a request
    CreateActivity (..),
    mkCreateActivity,

    -- ** Request lenses
    caName,
    caTags,

    -- * Destructuring the response
    CreateActivityResponse (..),
    mkCreateActivityResponse,

    -- ** Response lenses
    carsActivityARN,
    carsCreationDate,
    carsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StepFunctions.Types

-- | /See:/ 'mkCreateActivity' smart constructor.
data CreateActivity = CreateActivity'
  { -- | The name of the activity to create. This name must be unique for your AWS account and region for 90 days. For more information, see <https://docs.aws.amazon.com/step-functions/latest/dg/limits.html#service-limits-state-machine-executions Limits Related to State Machine Executions> in the /AWS Step Functions Developer Guide/ .
    --
    -- A name must /not/ contain:
    --
    --     * white space
    --
    --
    --     * brackets @< > { } [ ]@
    --
    --
    --     * wildcard characters @? *@
    --
    --
    --     * special characters @" # % \ ^ | ~ ` $ & , ; : /@
    --
    --
    --     * control characters (@U+0000-001F@ , @U+007F-009F@ )
    --
    --
    -- To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
    name :: Lude.Text,
    -- | The list of tags to add to a resource.
    --
    -- An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ , and <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags> .
    -- Tags may only contain Unicode letters, digits, white space, or these symbols: @_ . : / = + - @@ .
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateActivity' with the minimum fields required to make a request.
--
-- * 'name' - The name of the activity to create. This name must be unique for your AWS account and region for 90 days. For more information, see <https://docs.aws.amazon.com/step-functions/latest/dg/limits.html#service-limits-state-machine-executions Limits Related to State Machine Executions> in the /AWS Step Functions Developer Guide/ .
--
-- A name must /not/ contain:
--
--     * white space
--
--
--     * brackets @< > { } [ ]@
--
--
--     * wildcard characters @? *@
--
--
--     * special characters @" # % \ ^ | ~ ` $ & , ; : /@
--
--
--     * control characters (@U+0000-001F@ , @U+007F-009F@ )
--
--
-- To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
-- * 'tags' - The list of tags to add to a resource.
--
-- An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ , and <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags> .
-- Tags may only contain Unicode letters, digits, white space, or these symbols: @_ . : / = + - @@ .
mkCreateActivity ::
  -- | 'name'
  Lude.Text ->
  CreateActivity
mkCreateActivity pName_ =
  CreateActivity' {name = pName_, tags = Lude.Nothing}

-- | The name of the activity to create. This name must be unique for your AWS account and region for 90 days. For more information, see <https://docs.aws.amazon.com/step-functions/latest/dg/limits.html#service-limits-state-machine-executions Limits Related to State Machine Executions> in the /AWS Step Functions Developer Guide/ .
--
-- A name must /not/ contain:
--
--     * white space
--
--
--     * brackets @< > { } [ ]@
--
--
--     * wildcard characters @? *@
--
--
--     * special characters @" # % \ ^ | ~ ` $ & , ; : /@
--
--
--     * control characters (@U+0000-001F@ , @U+007F-009F@ )
--
--
-- To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caName :: Lens.Lens' CreateActivity Lude.Text
caName = Lens.lens (name :: CreateActivity -> Lude.Text) (\s a -> s {name = a} :: CreateActivity)
{-# DEPRECATED caName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The list of tags to add to a resource.
--
-- An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ , and <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags> .
-- Tags may only contain Unicode letters, digits, white space, or these symbols: @_ . : / = + - @@ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTags :: Lens.Lens' CreateActivity (Lude.Maybe [Tag])
caTags = Lens.lens (tags :: CreateActivity -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateActivity)
{-# DEPRECATED caTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateActivity where
  type Rs CreateActivity = CreateActivityResponse
  request = Req.postJSON stepFunctionsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateActivityResponse'
            Lude.<$> (x Lude..:> "activityArn")
            Lude.<*> (x Lude..:> "creationDate")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateActivity where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSStepFunctions.CreateActivity" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateActivity where
  toJSON CreateActivity' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("name" Lude..= name), ("tags" Lude..=) Lude.<$> tags]
      )

instance Lude.ToPath CreateActivity where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateActivity where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateActivityResponse' smart constructor.
data CreateActivityResponse = CreateActivityResponse'
  { -- | The Amazon Resource Name (ARN) that identifies the created activity.
    activityARN :: Lude.Text,
    -- | The date the activity is created.
    creationDate :: Lude.Timestamp,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateActivityResponse' with the minimum fields required to make a request.
--
-- * 'activityARN' - The Amazon Resource Name (ARN) that identifies the created activity.
-- * 'creationDate' - The date the activity is created.
-- * 'responseStatus' - The response status code.
mkCreateActivityResponse ::
  -- | 'activityARN'
  Lude.Text ->
  -- | 'creationDate'
  Lude.Timestamp ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateActivityResponse
mkCreateActivityResponse
  pActivityARN_
  pCreationDate_
  pResponseStatus_ =
    CreateActivityResponse'
      { activityARN = pActivityARN_,
        creationDate = pCreationDate_,
        responseStatus = pResponseStatus_
      }

-- | The Amazon Resource Name (ARN) that identifies the created activity.
--
-- /Note:/ Consider using 'activityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsActivityARN :: Lens.Lens' CreateActivityResponse Lude.Text
carsActivityARN = Lens.lens (activityARN :: CreateActivityResponse -> Lude.Text) (\s a -> s {activityARN = a} :: CreateActivityResponse)
{-# DEPRECATED carsActivityARN "Use generic-lens or generic-optics with 'activityARN' instead." #-}

-- | The date the activity is created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsCreationDate :: Lens.Lens' CreateActivityResponse Lude.Timestamp
carsCreationDate = Lens.lens (creationDate :: CreateActivityResponse -> Lude.Timestamp) (\s a -> s {creationDate = a} :: CreateActivityResponse)
{-# DEPRECATED carsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsResponseStatus :: Lens.Lens' CreateActivityResponse Lude.Int
carsResponseStatus = Lens.lens (responseStatus :: CreateActivityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateActivityResponse)
{-# DEPRECATED carsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

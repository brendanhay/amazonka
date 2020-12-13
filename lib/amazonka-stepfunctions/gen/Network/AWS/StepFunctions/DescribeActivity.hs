{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.DescribeActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an activity.
module Network.AWS.StepFunctions.DescribeActivity
  ( -- * Creating a request
    DescribeActivity (..),
    mkDescribeActivity,

    -- ** Request lenses
    daActivityARN,

    -- * Destructuring the response
    DescribeActivityResponse (..),
    mkDescribeActivityResponse,

    -- ** Response lenses
    darsActivityARN,
    darsName,
    darsCreationDate,
    darsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StepFunctions.Types

-- | /See:/ 'mkDescribeActivity' smart constructor.
newtype DescribeActivity = DescribeActivity'
  { -- | The Amazon Resource Name (ARN) of the activity to describe.
    activityARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeActivity' with the minimum fields required to make a request.
--
-- * 'activityARN' - The Amazon Resource Name (ARN) of the activity to describe.
mkDescribeActivity ::
  -- | 'activityARN'
  Lude.Text ->
  DescribeActivity
mkDescribeActivity pActivityARN_ =
  DescribeActivity' {activityARN = pActivityARN_}

-- | The Amazon Resource Name (ARN) of the activity to describe.
--
-- /Note:/ Consider using 'activityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daActivityARN :: Lens.Lens' DescribeActivity Lude.Text
daActivityARN = Lens.lens (activityARN :: DescribeActivity -> Lude.Text) (\s a -> s {activityARN = a} :: DescribeActivity)
{-# DEPRECATED daActivityARN "Use generic-lens or generic-optics with 'activityARN' instead." #-}

instance Lude.AWSRequest DescribeActivity where
  type Rs DescribeActivity = DescribeActivityResponse
  request = Req.postJSON stepFunctionsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeActivityResponse'
            Lude.<$> (x Lude..:> "activityArn")
            Lude.<*> (x Lude..:> "name")
            Lude.<*> (x Lude..:> "creationDate")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeActivity where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSStepFunctions.DescribeActivity" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeActivity where
  toJSON DescribeActivity' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("activityArn" Lude..= activityARN)])

instance Lude.ToPath DescribeActivity where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeActivity where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeActivityResponse' smart constructor.
data DescribeActivityResponse = DescribeActivityResponse'
  { -- | The Amazon Resource Name (ARN) that identifies the activity.
    activityARN :: Lude.Text,
    -- | The name of the activity.
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
    -- | The date the activity is created.
    creationDate :: Lude.Timestamp,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeActivityResponse' with the minimum fields required to make a request.
--
-- * 'activityARN' - The Amazon Resource Name (ARN) that identifies the activity.
-- * 'name' - The name of the activity.
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
-- * 'creationDate' - The date the activity is created.
-- * 'responseStatus' - The response status code.
mkDescribeActivityResponse ::
  -- | 'activityARN'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'creationDate'
  Lude.Timestamp ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeActivityResponse
mkDescribeActivityResponse
  pActivityARN_
  pName_
  pCreationDate_
  pResponseStatus_ =
    DescribeActivityResponse'
      { activityARN = pActivityARN_,
        name = pName_,
        creationDate = pCreationDate_,
        responseStatus = pResponseStatus_
      }

-- | The Amazon Resource Name (ARN) that identifies the activity.
--
-- /Note:/ Consider using 'activityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsActivityARN :: Lens.Lens' DescribeActivityResponse Lude.Text
darsActivityARN = Lens.lens (activityARN :: DescribeActivityResponse -> Lude.Text) (\s a -> s {activityARN = a} :: DescribeActivityResponse)
{-# DEPRECATED darsActivityARN "Use generic-lens or generic-optics with 'activityARN' instead." #-}

-- | The name of the activity.
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
darsName :: Lens.Lens' DescribeActivityResponse Lude.Text
darsName = Lens.lens (name :: DescribeActivityResponse -> Lude.Text) (\s a -> s {name = a} :: DescribeActivityResponse)
{-# DEPRECATED darsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The date the activity is created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsCreationDate :: Lens.Lens' DescribeActivityResponse Lude.Timestamp
darsCreationDate = Lens.lens (creationDate :: DescribeActivityResponse -> Lude.Timestamp) (\s a -> s {creationDate = a} :: DescribeActivityResponse)
{-# DEPRECATED darsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsResponseStatus :: Lens.Lens' DescribeActivityResponse Lude.Int
darsResponseStatus = Lens.lens (responseStatus :: DescribeActivityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeActivityResponse)
{-# DEPRECATED darsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

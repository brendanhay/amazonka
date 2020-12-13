{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.DataResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.DataResource
  ( DataResource (..),

    -- * Smart constructor
    mkDataResource,

    -- * Lenses
    drValues,
    drType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Amazon S3 buckets or AWS Lambda functions that you specify in your event selectors for your trail to log data events. Data events provide information about the resource operations performed on or within a resource itself. These are also known as data plane operations. You can specify up to 250 data resources for a trail.
--
-- The following example demonstrates how logging works when you configure logging of all data events for an S3 bucket named @bucket-1@ . In this example, the CloudTrail user specified an empty prefix, and the option to log both @Read@ and @Write@ data events.
--
--     * A user uploads an image file to @bucket-1@ .
--
--
--     * The @PutObject@ API operation is an Amazon S3 object-level API. It is recorded as a data event in CloudTrail. Because the CloudTrail user specified an S3 bucket with an empty prefix, events that occur on any object in that bucket are logged. The trail processes and logs the event.
--
--
--     * A user uploads an object to an Amazon S3 bucket named @arn:aws:s3:::bucket-2@ .
--
--
--     * The @PutObject@ API operation occurred for an object in an S3 bucket that the CloudTrail user didn't specify for the trail. The trail doesn’t log the event.
--
--
-- The following example demonstrates how logging works when you configure logging of AWS Lambda data events for a Lambda function named /MyLambdaFunction/ , but not for all AWS Lambda functions.
--
--     * A user runs a script that includes a call to the /MyLambdaFunction/ function and the /MyOtherLambdaFunction/ function.
--
--
--     * The @Invoke@ API operation on /MyLambdaFunction/ is an AWS Lambda API. It is recorded as a data event in CloudTrail. Because the CloudTrail user specified logging data events for /MyLambdaFunction/ , any invocations of that function are logged. The trail processes and logs the event.
--
--
--     * The @Invoke@ API operation on /MyOtherLambdaFunction/ is an AWS Lambda API. Because the CloudTrail user did not specify logging data events for all Lambda functions, the @Invoke@ operation for /MyOtherLambdaFunction/ does not match the function specified for the trail. The trail doesn’t log the event.
--
--
--
-- /See:/ 'mkDataResource' smart constructor.
data DataResource = DataResource'
  { -- | An array of Amazon Resource Name (ARN) strings or partial ARN strings for the specified objects.
    --
    --
    --     * To log data events for all objects in all S3 buckets in your AWS account, specify the prefix as @arn:aws:s3:::@ .
    --
    --
    --     * To log data events for all objects in an S3 bucket, specify the bucket and an empty object prefix such as @arn:aws:s3:::bucket-1/@ . The trail logs data events for all objects in this S3 bucket.
    --
    --
    --     * To log data events for specific objects, specify the S3 bucket and object prefix such as @arn:aws:s3:::bucket-1/example-images@ . The trail logs data events for objects in this S3 bucket that match the prefix.
    --
    --
    --     * To log data events for all functions in your AWS account, specify the prefix as @arn:aws:lambda@ .
    --
    --
    --     * To log data events for a specific Lambda function, specify the function ARN.
    values :: Lude.Maybe [Lude.Text],
    -- | The resource type in which you want to log data events. You can specify @AWS::S3::Object@ or @AWS::Lambda::Function@ resources.
    type' :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DataResource' with the minimum fields required to make a request.
--
-- * 'values' - An array of Amazon Resource Name (ARN) strings or partial ARN strings for the specified objects.
--
--
--     * To log data events for all objects in all S3 buckets in your AWS account, specify the prefix as @arn:aws:s3:::@ .
--
--
--     * To log data events for all objects in an S3 bucket, specify the bucket and an empty object prefix such as @arn:aws:s3:::bucket-1/@ . The trail logs data events for all objects in this S3 bucket.
--
--
--     * To log data events for specific objects, specify the S3 bucket and object prefix such as @arn:aws:s3:::bucket-1/example-images@ . The trail logs data events for objects in this S3 bucket that match the prefix.
--
--
--     * To log data events for all functions in your AWS account, specify the prefix as @arn:aws:lambda@ .
--
--
--     * To log data events for a specific Lambda function, specify the function ARN.
--
--
-- * 'type'' - The resource type in which you want to log data events. You can specify @AWS::S3::Object@ or @AWS::Lambda::Function@ resources.
mkDataResource ::
  DataResource
mkDataResource =
  DataResource' {values = Lude.Nothing, type' = Lude.Nothing}

-- | An array of Amazon Resource Name (ARN) strings or partial ARN strings for the specified objects.
--
--
--     * To log data events for all objects in all S3 buckets in your AWS account, specify the prefix as @arn:aws:s3:::@ .
--
--
--     * To log data events for all objects in an S3 bucket, specify the bucket and an empty object prefix such as @arn:aws:s3:::bucket-1/@ . The trail logs data events for all objects in this S3 bucket.
--
--
--     * To log data events for specific objects, specify the S3 bucket and object prefix such as @arn:aws:s3:::bucket-1/example-images@ . The trail logs data events for objects in this S3 bucket that match the prefix.
--
--
--     * To log data events for all functions in your AWS account, specify the prefix as @arn:aws:lambda@ .
--
--
--     * To log data events for a specific Lambda function, specify the function ARN.
--
--
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drValues :: Lens.Lens' DataResource (Lude.Maybe [Lude.Text])
drValues = Lens.lens (values :: DataResource -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: DataResource)
{-# DEPRECATED drValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The resource type in which you want to log data events. You can specify @AWS::S3::Object@ or @AWS::Lambda::Function@ resources.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drType :: Lens.Lens' DataResource (Lude.Maybe Lude.Text)
drType = Lens.lens (type' :: DataResource -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: DataResource)
{-# DEPRECATED drType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON DataResource where
  parseJSON =
    Lude.withObject
      "DataResource"
      ( \x ->
          DataResource'
            Lude.<$> (x Lude..:? "Values" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Type")
      )

instance Lude.ToJSON DataResource where
  toJSON DataResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Values" Lude..=) Lude.<$> values,
            ("Type" Lude..=) Lude.<$> type'
          ]
      )

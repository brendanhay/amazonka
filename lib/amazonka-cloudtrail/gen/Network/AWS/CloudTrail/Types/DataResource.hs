{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.DataResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.DataResource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Amazon S3 buckets or AWS Lambda functions that you specify in your event selectors for your trail to log data events. Data events provide information about the resource operations performed on or within a resource itself. These are also known as data plane operations. You can specify up to 250 data resources for a trail.
--
--
-- The following example demonstrates how logging works when you configure logging of all data events for an S3 bucket named @bucket-1@ . In this example, the CloudTrail user specified an empty prefix, and the option to log both @Read@ and @Write@ data events.
--
--     * A user uploads an image file to @bucket-1@ .
--
--     * The @PutObject@ API operation is an Amazon S3 object-level API. It is recorded as a data event in CloudTrail. Because the CloudTrail user specified an S3 bucket with an empty prefix, events that occur on any object in that bucket are logged. The trail processes and logs the event.
--
--     * A user uploads an object to an Amazon S3 bucket named @arn:aws:s3:::bucket-2@ .
--
--     * The @PutObject@ API operation occurred for an object in an S3 bucket that the CloudTrail user didn't specify for the trail. The trail doesn’t log the event.
--
--
--
-- The following example demonstrates how logging works when you configure logging of AWS Lambda data events for a Lambda function named /MyLambdaFunction/ , but not for all AWS Lambda functions.
--
--     * A user runs a script that includes a call to the /MyLambdaFunction/ function and the /MyOtherLambdaFunction/ function.
--
--     * The @Invoke@ API operation on /MyLambdaFunction/ is an AWS Lambda API. It is recorded as a data event in CloudTrail. Because the CloudTrail user specified logging data events for /MyLambdaFunction/ , any invocations of that function are logged. The trail processes and logs the event.
--
--     * The @Invoke@ API operation on /MyOtherLambdaFunction/ is an AWS Lambda API. Because the CloudTrail user did not specify logging data events for all Lambda functions, the @Invoke@ operation for /MyOtherLambdaFunction/ does not match the function specified for the trail. The trail doesn’t log the event.
--
--
--
--
-- /See:/ 'dataResource' smart constructor.
data DataResource = DataResource'
  { _drValues :: !(Maybe [Text]),
    _drType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DataResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drValues' - An array of Amazon Resource Name (ARN) strings or partial ARN strings for the specified objects.     * To log data events for all objects in all S3 buckets in your AWS account, specify the prefix as @arn:aws:s3:::@ .      * To log data events for all objects in an S3 bucket, specify the bucket and an empty object prefix such as @arn:aws:s3:::bucket-1/@ . The trail logs data events for all objects in this S3 bucket.     * To log data events for specific objects, specify the S3 bucket and object prefix such as @arn:aws:s3:::bucket-1/example-images@ . The trail logs data events for objects in this S3 bucket that match the prefix.     * To log data events for all functions in your AWS account, specify the prefix as @arn:aws:lambda@ .     * To log data events for a specific Lambda function, specify the function ARN.
--
-- * 'drType' - The resource type in which you want to log data events. You can specify @AWS::S3::Object@ or @AWS::Lambda::Function@ resources.
dataResource ::
  DataResource
dataResource =
  DataResource' {_drValues = Nothing, _drType = Nothing}

-- | An array of Amazon Resource Name (ARN) strings or partial ARN strings for the specified objects.     * To log data events for all objects in all S3 buckets in your AWS account, specify the prefix as @arn:aws:s3:::@ .      * To log data events for all objects in an S3 bucket, specify the bucket and an empty object prefix such as @arn:aws:s3:::bucket-1/@ . The trail logs data events for all objects in this S3 bucket.     * To log data events for specific objects, specify the S3 bucket and object prefix such as @arn:aws:s3:::bucket-1/example-images@ . The trail logs data events for objects in this S3 bucket that match the prefix.     * To log data events for all functions in your AWS account, specify the prefix as @arn:aws:lambda@ .     * To log data events for a specific Lambda function, specify the function ARN.
drValues :: Lens' DataResource [Text]
drValues = lens _drValues (\s a -> s {_drValues = a}) . _Default . _Coerce

-- | The resource type in which you want to log data events. You can specify @AWS::S3::Object@ or @AWS::Lambda::Function@ resources.
drType :: Lens' DataResource (Maybe Text)
drType = lens _drType (\s a -> s {_drType = a})

instance FromJSON DataResource where
  parseJSON =
    withObject
      "DataResource"
      ( \x ->
          DataResource' <$> (x .:? "Values" .!= mempty) <*> (x .:? "Type")
      )

instance Hashable DataResource

instance NFData DataResource

instance ToJSON DataResource where
  toJSON DataResource' {..} =
    object
      (catMaybes [("Values" .=) <$> _drValues, ("Type" .=) <$> _drType])

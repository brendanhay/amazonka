{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudTrail.Types.DataResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.DataResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Amazon S3 buckets, Lambda functions, or Amazon DynamoDB tables that
-- you specify in your event selectors for your trail to log data events.
-- Data events provide information about the resource operations performed
-- on or within a resource itself. These are also known as data plane
-- operations. You can specify up to 250 data resources for a trail.
--
-- The total number of allowed data resources is 250. This number can be
-- distributed between 1 and 5 event selectors, but the total cannot exceed
-- 250 across all selectors.
--
-- If you are using advanced event selectors, the maximum total number of
-- values for all conditions, across all advanced event selectors for the
-- trail, is 500.
--
-- The following example demonstrates how logging works when you configure
-- logging of all data events for an S3 bucket named @bucket-1@. In this
-- example, the CloudTrail user specified an empty prefix, and the option
-- to log both @Read@ and @Write@ data events.
--
-- 1.  A user uploads an image file to @bucket-1@.
--
-- 2.  The @PutObject@ API operation is an Amazon S3 object-level API. It
--     is recorded as a data event in CloudTrail. Because the CloudTrail
--     user specified an S3 bucket with an empty prefix, events that occur
--     on any object in that bucket are logged. The trail processes and
--     logs the event.
--
-- 3.  A user uploads an object to an Amazon S3 bucket named
--     @arn:aws:s3:::bucket-2@.
--
-- 4.  The @PutObject@ API operation occurred for an object in an S3 bucket
--     that the CloudTrail user didn\'t specify for the trail. The trail
--     doesn’t log the event.
--
-- The following example demonstrates how logging works when you configure
-- logging of Lambda data events for a Lambda function named
-- /MyLambdaFunction/, but not for all Lambda functions.
--
-- 1.  A user runs a script that includes a call to the /MyLambdaFunction/
--     function and the /MyOtherLambdaFunction/ function.
--
-- 2.  The @Invoke@ API operation on /MyLambdaFunction/ is an Lambda API.
--     It is recorded as a data event in CloudTrail. Because the CloudTrail
--     user specified logging data events for /MyLambdaFunction/, any
--     invocations of that function are logged. The trail processes and
--     logs the event.
--
-- 3.  The @Invoke@ API operation on /MyOtherLambdaFunction/ is an Lambda
--     API. Because the CloudTrail user did not specify logging data events
--     for all Lambda functions, the @Invoke@ operation for
--     /MyOtherLambdaFunction/ does not match the function specified for
--     the trail. The trail doesn’t log the event.
--
-- /See:/ 'newDataResource' smart constructor.
data DataResource = DataResource'
  { -- | The resource type in which you want to log data events. You can specify
    -- the following /basic/ event selector resource types:
    --
    -- -   @AWS::S3::Object@
    --
    -- -   @AWS::Lambda::Function@
    --
    -- -   @AWS::DynamoDB::Table@
    --
    -- The following resource types are also available through /advanced/ event
    -- selectors. Basic event selector resource types are valid in advanced
    -- event selectors, but advanced event selector resource types are not
    -- valid in basic event selectors. For more information, see
    -- AdvancedFieldSelector$Field.
    --
    -- -   @AWS::S3Outposts::Object@
    --
    -- -   @AWS::ManagedBlockchain::Node@
    --
    -- -   @AWS::S3ObjectLambda::AccessPoint@
    --
    -- -   @AWS::EC2::Snapshot@
    --
    -- -   @AWS::S3::AccessPoint@
    --
    -- -   @AWS::DynamoDB::Stream@
    --
    -- -   @AWS::Glue::Table@
    type' :: Prelude.Maybe Prelude.Text,
    -- | An array of Amazon Resource Name (ARN) strings or partial ARN strings
    -- for the specified objects.
    --
    -- -   To log data events for all objects in all S3 buckets in your Amazon
    --     Web Services account, specify the prefix as @arn:aws:s3@.
    --
    --     This also enables logging of data event activity performed by any
    --     user or role in your Amazon Web Services account, even if that
    --     activity is performed on a bucket that belongs to another Amazon Web
    --     Services account.
    --
    -- -   To log data events for all objects in an S3 bucket, specify the
    --     bucket and an empty object prefix such as @arn:aws:s3:::bucket-1\/@.
    --     The trail logs data events for all objects in this S3 bucket.
    --
    -- -   To log data events for specific objects, specify the S3 bucket and
    --     object prefix such as @arn:aws:s3:::bucket-1\/example-images@. The
    --     trail logs data events for objects in this S3 bucket that match the
    --     prefix.
    --
    -- -   To log data events for all Lambda functions in your Amazon Web
    --     Services account, specify the prefix as @arn:aws:lambda@.
    --
    --     This also enables logging of @Invoke@ activity performed by any user
    --     or role in your Amazon Web Services account, even if that activity
    --     is performed on a function that belongs to another Amazon Web
    --     Services account.
    --
    -- -   To log data events for a specific Lambda function, specify the
    --     function ARN.
    --
    --     Lambda function ARNs are exact. For example, if you specify a
    --     function ARN
    --     /arn:aws:lambda:us-west-2:111111111111:function:helloworld/, data
    --     events will only be logged for
    --     /arn:aws:lambda:us-west-2:111111111111:function:helloworld/. They
    --     will not be logged for
    --     /arn:aws:lambda:us-west-2:111111111111:function:helloworld2/.
    --
    -- -   To log data events for all DynamoDB tables in your Amazon Web
    --     Services account, specify the prefix as @arn:aws:dynamodb@.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'dataResource_type' - The resource type in which you want to log data events. You can specify
-- the following /basic/ event selector resource types:
--
-- -   @AWS::S3::Object@
--
-- -   @AWS::Lambda::Function@
--
-- -   @AWS::DynamoDB::Table@
--
-- The following resource types are also available through /advanced/ event
-- selectors. Basic event selector resource types are valid in advanced
-- event selectors, but advanced event selector resource types are not
-- valid in basic event selectors. For more information, see
-- AdvancedFieldSelector$Field.
--
-- -   @AWS::S3Outposts::Object@
--
-- -   @AWS::ManagedBlockchain::Node@
--
-- -   @AWS::S3ObjectLambda::AccessPoint@
--
-- -   @AWS::EC2::Snapshot@
--
-- -   @AWS::S3::AccessPoint@
--
-- -   @AWS::DynamoDB::Stream@
--
-- -   @AWS::Glue::Table@
--
-- 'values', 'dataResource_values' - An array of Amazon Resource Name (ARN) strings or partial ARN strings
-- for the specified objects.
--
-- -   To log data events for all objects in all S3 buckets in your Amazon
--     Web Services account, specify the prefix as @arn:aws:s3@.
--
--     This also enables logging of data event activity performed by any
--     user or role in your Amazon Web Services account, even if that
--     activity is performed on a bucket that belongs to another Amazon Web
--     Services account.
--
-- -   To log data events for all objects in an S3 bucket, specify the
--     bucket and an empty object prefix such as @arn:aws:s3:::bucket-1\/@.
--     The trail logs data events for all objects in this S3 bucket.
--
-- -   To log data events for specific objects, specify the S3 bucket and
--     object prefix such as @arn:aws:s3:::bucket-1\/example-images@. The
--     trail logs data events for objects in this S3 bucket that match the
--     prefix.
--
-- -   To log data events for all Lambda functions in your Amazon Web
--     Services account, specify the prefix as @arn:aws:lambda@.
--
--     This also enables logging of @Invoke@ activity performed by any user
--     or role in your Amazon Web Services account, even if that activity
--     is performed on a function that belongs to another Amazon Web
--     Services account.
--
-- -   To log data events for a specific Lambda function, specify the
--     function ARN.
--
--     Lambda function ARNs are exact. For example, if you specify a
--     function ARN
--     /arn:aws:lambda:us-west-2:111111111111:function:helloworld/, data
--     events will only be logged for
--     /arn:aws:lambda:us-west-2:111111111111:function:helloworld/. They
--     will not be logged for
--     /arn:aws:lambda:us-west-2:111111111111:function:helloworld2/.
--
-- -   To log data events for all DynamoDB tables in your Amazon Web
--     Services account, specify the prefix as @arn:aws:dynamodb@.
newDataResource ::
  DataResource
newDataResource =
  DataResource'
    { type' = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The resource type in which you want to log data events. You can specify
-- the following /basic/ event selector resource types:
--
-- -   @AWS::S3::Object@
--
-- -   @AWS::Lambda::Function@
--
-- -   @AWS::DynamoDB::Table@
--
-- The following resource types are also available through /advanced/ event
-- selectors. Basic event selector resource types are valid in advanced
-- event selectors, but advanced event selector resource types are not
-- valid in basic event selectors. For more information, see
-- AdvancedFieldSelector$Field.
--
-- -   @AWS::S3Outposts::Object@
--
-- -   @AWS::ManagedBlockchain::Node@
--
-- -   @AWS::S3ObjectLambda::AccessPoint@
--
-- -   @AWS::EC2::Snapshot@
--
-- -   @AWS::S3::AccessPoint@
--
-- -   @AWS::DynamoDB::Stream@
--
-- -   @AWS::Glue::Table@
dataResource_type :: Lens.Lens' DataResource (Prelude.Maybe Prelude.Text)
dataResource_type = Lens.lens (\DataResource' {type'} -> type') (\s@DataResource' {} a -> s {type' = a} :: DataResource)

-- | An array of Amazon Resource Name (ARN) strings or partial ARN strings
-- for the specified objects.
--
-- -   To log data events for all objects in all S3 buckets in your Amazon
--     Web Services account, specify the prefix as @arn:aws:s3@.
--
--     This also enables logging of data event activity performed by any
--     user or role in your Amazon Web Services account, even if that
--     activity is performed on a bucket that belongs to another Amazon Web
--     Services account.
--
-- -   To log data events for all objects in an S3 bucket, specify the
--     bucket and an empty object prefix such as @arn:aws:s3:::bucket-1\/@.
--     The trail logs data events for all objects in this S3 bucket.
--
-- -   To log data events for specific objects, specify the S3 bucket and
--     object prefix such as @arn:aws:s3:::bucket-1\/example-images@. The
--     trail logs data events for objects in this S3 bucket that match the
--     prefix.
--
-- -   To log data events for all Lambda functions in your Amazon Web
--     Services account, specify the prefix as @arn:aws:lambda@.
--
--     This also enables logging of @Invoke@ activity performed by any user
--     or role in your Amazon Web Services account, even if that activity
--     is performed on a function that belongs to another Amazon Web
--     Services account.
--
-- -   To log data events for a specific Lambda function, specify the
--     function ARN.
--
--     Lambda function ARNs are exact. For example, if you specify a
--     function ARN
--     /arn:aws:lambda:us-west-2:111111111111:function:helloworld/, data
--     events will only be logged for
--     /arn:aws:lambda:us-west-2:111111111111:function:helloworld/. They
--     will not be logged for
--     /arn:aws:lambda:us-west-2:111111111111:function:helloworld2/.
--
-- -   To log data events for all DynamoDB tables in your Amazon Web
--     Services account, specify the prefix as @arn:aws:dynamodb@.
dataResource_values :: Lens.Lens' DataResource (Prelude.Maybe [Prelude.Text])
dataResource_values = Lens.lens (\DataResource' {values} -> values) (\s@DataResource' {} a -> s {values = a} :: DataResource) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON DataResource where
  parseJSON =
    Core.withObject
      "DataResource"
      ( \x ->
          DataResource'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Values" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable DataResource where
  hashWithSalt _salt DataResource' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` values

instance Prelude.NFData DataResource where
  rnf DataResource' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf values

instance Core.ToJSON DataResource where
  toJSON DataResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Type" Core..=) Prelude.<$> type',
            ("Values" Core..=) Prelude.<$> values
          ]
      )

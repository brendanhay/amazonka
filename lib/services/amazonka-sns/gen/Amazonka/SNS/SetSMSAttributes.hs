{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SNS.SetSMSAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this request to set the default settings for sending SMS messages
-- and receiving daily SMS usage reports.
--
-- You can override some of these settings for a single message when you
-- use the @Publish@ action with the @MessageAttributes.entry.N@ parameter.
-- For more information, see
-- <https://docs.aws.amazon.com/sns/latest/dg/sms_publish-to-phone.html Publishing to a mobile phone>
-- in the /Amazon SNS Developer Guide/.
--
-- To use this operation, you must grant the Amazon SNS service principal
-- (@sns.amazonaws.com@) permission to perform the @s3:ListBucket@ action.
module Amazonka.SNS.SetSMSAttributes
  ( -- * Creating a Request
    SetSMSAttributes (..),
    newSetSMSAttributes,

    -- * Request Lenses
    setSMSAttributes_attributes,

    -- * Destructuring the Response
    SetSMSAttributesResponse (..),
    newSetSMSAttributesResponse,

    -- * Response Lenses
    setSMSAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | The input for the SetSMSAttributes action.
--
-- /See:/ 'newSetSMSAttributes' smart constructor.
data SetSMSAttributes = SetSMSAttributes'
  { -- | The default settings for sending SMS messages from your Amazon Web
    -- Services account. You can set values for the following attribute names:
    --
    -- @MonthlySpendLimit@ – The maximum amount in USD that you are willing to
    -- spend each month to send SMS messages. When Amazon SNS determines that
    -- sending an SMS message would incur a cost that exceeds this limit, it
    -- stops sending SMS messages within minutes.
    --
    -- Amazon SNS stops sending SMS messages within minutes of the limit being
    -- crossed. During that interval, if you continue to send SMS messages, you
    -- will incur costs that exceed your limit.
    --
    -- By default, the spend limit is set to the maximum allowed by Amazon SNS.
    -- If you want to raise the limit, submit an
    -- <https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&limitType=service-code-sns SNS Limit Increase case>.
    -- For __New limit value__, enter your desired monthly spend limit. In the
    -- __Use Case Description__ field, explain that you are requesting an SMS
    -- monthly spend limit increase.
    --
    -- @DeliveryStatusIAMRole@ – The ARN of the IAM role that allows Amazon SNS
    -- to write logs about SMS deliveries in CloudWatch Logs. For each SMS
    -- message that you send, Amazon SNS writes a log that includes the message
    -- price, the success or failure status, the reason for failure (if the
    -- message failed), the message dwell time, and other information.
    --
    -- @DeliveryStatusSuccessSamplingRate@ – The percentage of successful SMS
    -- deliveries for which Amazon SNS will write logs in CloudWatch Logs. The
    -- value can be an integer from 0 - 100. For example, to write logs only
    -- for failed deliveries, set this value to @0@. To write logs for 10% of
    -- your successful deliveries, set it to @10@.
    --
    -- @DefaultSenderID@ – A string, such as your business brand, that is
    -- displayed as the sender on the receiving device. Support for sender IDs
    -- varies by country. The sender ID can be 1 - 11 alphanumeric characters,
    -- and it must contain at least one letter.
    --
    -- @DefaultSMSType@ – The type of SMS message that you will send by
    -- default. You can assign the following values:
    --
    -- -   @Promotional@ – (Default) Noncritical messages, such as marketing
    --     messages. Amazon SNS optimizes the message delivery to incur the
    --     lowest cost.
    --
    -- -   @Transactional@ – Critical messages that support customer
    --     transactions, such as one-time passcodes for multi-factor
    --     authentication. Amazon SNS optimizes the message delivery to achieve
    --     the highest reliability.
    --
    -- @UsageReportS3Bucket@ – The name of the Amazon S3 bucket to receive
    -- daily SMS usage reports from Amazon SNS. Each day, Amazon SNS will
    -- deliver a usage report as a CSV file to the bucket. The report includes
    -- the following information for each SMS message that was successfully
    -- delivered by your Amazon Web Services account:
    --
    -- -   Time that the message was published (in UTC)
    --
    -- -   Message ID
    --
    -- -   Destination phone number
    --
    -- -   Message type
    --
    -- -   Delivery status
    --
    -- -   Message price (in USD)
    --
    -- -   Part number (a message is split into multiple parts if it is too
    --     long for a single message)
    --
    -- -   Total number of parts
    --
    -- To receive the report, the bucket must have a policy that allows the
    -- Amazon SNS service principal to perform the @s3:PutObject@ and
    -- @s3:GetBucketLocation@ actions.
    --
    -- For an example bucket policy and usage report, see
    -- <https://docs.aws.amazon.com/sns/latest/dg/sms_stats.html Monitoring SMS Activity>
    -- in the /Amazon SNS Developer Guide/.
    attributes :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetSMSAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'setSMSAttributes_attributes' - The default settings for sending SMS messages from your Amazon Web
-- Services account. You can set values for the following attribute names:
--
-- @MonthlySpendLimit@ – The maximum amount in USD that you are willing to
-- spend each month to send SMS messages. When Amazon SNS determines that
-- sending an SMS message would incur a cost that exceeds this limit, it
-- stops sending SMS messages within minutes.
--
-- Amazon SNS stops sending SMS messages within minutes of the limit being
-- crossed. During that interval, if you continue to send SMS messages, you
-- will incur costs that exceed your limit.
--
-- By default, the spend limit is set to the maximum allowed by Amazon SNS.
-- If you want to raise the limit, submit an
-- <https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&limitType=service-code-sns SNS Limit Increase case>.
-- For __New limit value__, enter your desired monthly spend limit. In the
-- __Use Case Description__ field, explain that you are requesting an SMS
-- monthly spend limit increase.
--
-- @DeliveryStatusIAMRole@ – The ARN of the IAM role that allows Amazon SNS
-- to write logs about SMS deliveries in CloudWatch Logs. For each SMS
-- message that you send, Amazon SNS writes a log that includes the message
-- price, the success or failure status, the reason for failure (if the
-- message failed), the message dwell time, and other information.
--
-- @DeliveryStatusSuccessSamplingRate@ – The percentage of successful SMS
-- deliveries for which Amazon SNS will write logs in CloudWatch Logs. The
-- value can be an integer from 0 - 100. For example, to write logs only
-- for failed deliveries, set this value to @0@. To write logs for 10% of
-- your successful deliveries, set it to @10@.
--
-- @DefaultSenderID@ – A string, such as your business brand, that is
-- displayed as the sender on the receiving device. Support for sender IDs
-- varies by country. The sender ID can be 1 - 11 alphanumeric characters,
-- and it must contain at least one letter.
--
-- @DefaultSMSType@ – The type of SMS message that you will send by
-- default. You can assign the following values:
--
-- -   @Promotional@ – (Default) Noncritical messages, such as marketing
--     messages. Amazon SNS optimizes the message delivery to incur the
--     lowest cost.
--
-- -   @Transactional@ – Critical messages that support customer
--     transactions, such as one-time passcodes for multi-factor
--     authentication. Amazon SNS optimizes the message delivery to achieve
--     the highest reliability.
--
-- @UsageReportS3Bucket@ – The name of the Amazon S3 bucket to receive
-- daily SMS usage reports from Amazon SNS. Each day, Amazon SNS will
-- deliver a usage report as a CSV file to the bucket. The report includes
-- the following information for each SMS message that was successfully
-- delivered by your Amazon Web Services account:
--
-- -   Time that the message was published (in UTC)
--
-- -   Message ID
--
-- -   Destination phone number
--
-- -   Message type
--
-- -   Delivery status
--
-- -   Message price (in USD)
--
-- -   Part number (a message is split into multiple parts if it is too
--     long for a single message)
--
-- -   Total number of parts
--
-- To receive the report, the bucket must have a policy that allows the
-- Amazon SNS service principal to perform the @s3:PutObject@ and
-- @s3:GetBucketLocation@ actions.
--
-- For an example bucket policy and usage report, see
-- <https://docs.aws.amazon.com/sns/latest/dg/sms_stats.html Monitoring SMS Activity>
-- in the /Amazon SNS Developer Guide/.
newSetSMSAttributes ::
  SetSMSAttributes
newSetSMSAttributes =
  SetSMSAttributes' {attributes = Prelude.mempty}

-- | The default settings for sending SMS messages from your Amazon Web
-- Services account. You can set values for the following attribute names:
--
-- @MonthlySpendLimit@ – The maximum amount in USD that you are willing to
-- spend each month to send SMS messages. When Amazon SNS determines that
-- sending an SMS message would incur a cost that exceeds this limit, it
-- stops sending SMS messages within minutes.
--
-- Amazon SNS stops sending SMS messages within minutes of the limit being
-- crossed. During that interval, if you continue to send SMS messages, you
-- will incur costs that exceed your limit.
--
-- By default, the spend limit is set to the maximum allowed by Amazon SNS.
-- If you want to raise the limit, submit an
-- <https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&limitType=service-code-sns SNS Limit Increase case>.
-- For __New limit value__, enter your desired monthly spend limit. In the
-- __Use Case Description__ field, explain that you are requesting an SMS
-- monthly spend limit increase.
--
-- @DeliveryStatusIAMRole@ – The ARN of the IAM role that allows Amazon SNS
-- to write logs about SMS deliveries in CloudWatch Logs. For each SMS
-- message that you send, Amazon SNS writes a log that includes the message
-- price, the success or failure status, the reason for failure (if the
-- message failed), the message dwell time, and other information.
--
-- @DeliveryStatusSuccessSamplingRate@ – The percentage of successful SMS
-- deliveries for which Amazon SNS will write logs in CloudWatch Logs. The
-- value can be an integer from 0 - 100. For example, to write logs only
-- for failed deliveries, set this value to @0@. To write logs for 10% of
-- your successful deliveries, set it to @10@.
--
-- @DefaultSenderID@ – A string, such as your business brand, that is
-- displayed as the sender on the receiving device. Support for sender IDs
-- varies by country. The sender ID can be 1 - 11 alphanumeric characters,
-- and it must contain at least one letter.
--
-- @DefaultSMSType@ – The type of SMS message that you will send by
-- default. You can assign the following values:
--
-- -   @Promotional@ – (Default) Noncritical messages, such as marketing
--     messages. Amazon SNS optimizes the message delivery to incur the
--     lowest cost.
--
-- -   @Transactional@ – Critical messages that support customer
--     transactions, such as one-time passcodes for multi-factor
--     authentication. Amazon SNS optimizes the message delivery to achieve
--     the highest reliability.
--
-- @UsageReportS3Bucket@ – The name of the Amazon S3 bucket to receive
-- daily SMS usage reports from Amazon SNS. Each day, Amazon SNS will
-- deliver a usage report as a CSV file to the bucket. The report includes
-- the following information for each SMS message that was successfully
-- delivered by your Amazon Web Services account:
--
-- -   Time that the message was published (in UTC)
--
-- -   Message ID
--
-- -   Destination phone number
--
-- -   Message type
--
-- -   Delivery status
--
-- -   Message price (in USD)
--
-- -   Part number (a message is split into multiple parts if it is too
--     long for a single message)
--
-- -   Total number of parts
--
-- To receive the report, the bucket must have a policy that allows the
-- Amazon SNS service principal to perform the @s3:PutObject@ and
-- @s3:GetBucketLocation@ actions.
--
-- For an example bucket policy and usage report, see
-- <https://docs.aws.amazon.com/sns/latest/dg/sms_stats.html Monitoring SMS Activity>
-- in the /Amazon SNS Developer Guide/.
setSMSAttributes_attributes :: Lens.Lens' SetSMSAttributes (Prelude.HashMap Prelude.Text Prelude.Text)
setSMSAttributes_attributes = Lens.lens (\SetSMSAttributes' {attributes} -> attributes) (\s@SetSMSAttributes' {} a -> s {attributes = a} :: SetSMSAttributes) Prelude.. Lens.coerced

instance Core.AWSRequest SetSMSAttributes where
  type
    AWSResponse SetSMSAttributes =
      SetSMSAttributesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "SetSMSAttributesResult"
      ( \s h x ->
          SetSMSAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetSMSAttributes where
  hashWithSalt _salt SetSMSAttributes' {..} =
    _salt `Prelude.hashWithSalt` attributes

instance Prelude.NFData SetSMSAttributes where
  rnf SetSMSAttributes' {..} = Prelude.rnf attributes

instance Data.ToHeaders SetSMSAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SetSMSAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery SetSMSAttributes where
  toQuery SetSMSAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("SetSMSAttributes" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-03-31" :: Prelude.ByteString),
        "attributes"
          Data.=: Data.toQueryMap "entry" "key" "value" attributes
      ]

-- | The response for the SetSMSAttributes action.
--
-- /See:/ 'newSetSMSAttributesResponse' smart constructor.
data SetSMSAttributesResponse = SetSMSAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetSMSAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'setSMSAttributesResponse_httpStatus' - The response's http status code.
newSetSMSAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetSMSAttributesResponse
newSetSMSAttributesResponse pHttpStatus_ =
  SetSMSAttributesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
setSMSAttributesResponse_httpStatus :: Lens.Lens' SetSMSAttributesResponse Prelude.Int
setSMSAttributesResponse_httpStatus = Lens.lens (\SetSMSAttributesResponse' {httpStatus} -> httpStatus) (\s@SetSMSAttributesResponse' {} a -> s {httpStatus = a} :: SetSMSAttributesResponse)

instance Prelude.NFData SetSMSAttributesResponse where
  rnf SetSMSAttributesResponse' {..} =
    Prelude.rnf httpStatus

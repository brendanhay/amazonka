{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.SetSMSAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this request to set the default settings for sending SMS messages and receiving daily SMS usage reports.
--
-- You can override some of these settings for a single message when you use the @Publish@ action with the @MessageAttributes.entry.N@ parameter. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/sms_publish-to-phone.html Publishing to a mobile phone> in the /Amazon SNS Developer Guide/ .
module Network.AWS.SNS.SetSMSAttributes
  ( -- * Creating a request
    SetSMSAttributes (..),
    mkSetSMSAttributes,

    -- ** Request lenses
    ssmsaAttributes,

    -- * Destructuring the response
    SetSMSAttributesResponse (..),
    mkSetSMSAttributesResponse,

    -- ** Response lenses
    ssmsarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | The input for the SetSMSAttributes action.
--
-- /See:/ 'mkSetSMSAttributes' smart constructor.
newtype SetSMSAttributes = SetSMSAttributes'
  { -- | The default settings for sending SMS messages from your account. You can set values for the following attribute names:
    --
    -- @MonthlySpendLimit@ – The maximum amount in USD that you are willing to spend each month to send SMS messages. When Amazon SNS determines that sending an SMS message would incur a cost that exceeds this limit, it stops sending SMS messages within minutes.
    -- /Important:/ Amazon SNS stops sending SMS messages within minutes of the limit being crossed. During that interval, if you continue to send SMS messages, you will incur costs that exceed your limit.
    -- By default, the spend limit is set to the maximum allowed by Amazon SNS. If you want to raise the limit, submit an <https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&limitType=service-code-sns SNS Limit Increase case> . For __New limit value__ , enter your desired monthly spend limit. In the __Use Case Description__ field, explain that you are requesting an SMS monthly spend limit increase.
    -- @DeliveryStatusIAMRole@ – The ARN of the IAM role that allows Amazon SNS to write logs about SMS deliveries in CloudWatch Logs. For each SMS message that you send, Amazon SNS writes a log that includes the message price, the success or failure status, the reason for failure (if the message failed), the message dwell time, and other information.
    -- @DeliveryStatusSuccessSamplingRate@ – The percentage of successful SMS deliveries for which Amazon SNS will write logs in CloudWatch Logs. The value can be an integer from 0 - 100. For example, to write logs only for failed deliveries, set this value to @0@ . To write logs for 10% of your successful deliveries, set it to @10@ .
    -- @DefaultSenderID@ – A string, such as your business brand, that is displayed as the sender on the receiving device. Support for sender IDs varies by country. The sender ID can be 1 - 11 alphanumeric characters, and it must contain at least one letter.
    -- @DefaultSMSType@ – The type of SMS message that you will send by default. You can assign the following values:
    --
    --     * @Promotional@ – (Default) Noncritical messages, such as marketing messages. Amazon SNS optimizes the message delivery to incur the lowest cost.
    --
    --
    --     * @Transactional@ – Critical messages that support customer transactions, such as one-time passcodes for multi-factor authentication. Amazon SNS optimizes the message delivery to achieve the highest reliability.
    --
    --
    -- @UsageReportS3Bucket@ – The name of the Amazon S3 bucket to receive daily SMS usage reports from Amazon SNS. Each day, Amazon SNS will deliver a usage report as a CSV file to the bucket. The report includes the following information for each SMS message that was successfully delivered by your account:
    --
    --     * Time that the message was published (in UTC)
    --
    --
    --     * Message ID
    --
    --
    --     * Destination phone number
    --
    --
    --     * Message type
    --
    --
    --     * Delivery status
    --
    --
    --     * Message price (in USD)
    --
    --
    --     * Part number (a message is split into multiple parts if it is too long for a single message)
    --
    --
    --     * Total number of parts
    --
    --
    -- To receive the report, the bucket must have a policy that allows the Amazon SNS service principle to perform the @s3:PutObject@ and @s3:GetBucketLocation@ actions.
    -- For an example bucket policy and usage report, see <https://docs.aws.amazon.com/sns/latest/dg/sms_stats.html Monitoring SMS Activity> in the /Amazon SNS Developer Guide/ .
    attributes :: Core.HashMap Types.String Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetSMSAttributes' value with any optional fields omitted.
mkSetSMSAttributes ::
  SetSMSAttributes
mkSetSMSAttributes = SetSMSAttributes' {attributes = Core.mempty}

-- | The default settings for sending SMS messages from your account. You can set values for the following attribute names:
--
-- @MonthlySpendLimit@ – The maximum amount in USD that you are willing to spend each month to send SMS messages. When Amazon SNS determines that sending an SMS message would incur a cost that exceeds this limit, it stops sending SMS messages within minutes.
-- /Important:/ Amazon SNS stops sending SMS messages within minutes of the limit being crossed. During that interval, if you continue to send SMS messages, you will incur costs that exceed your limit.
-- By default, the spend limit is set to the maximum allowed by Amazon SNS. If you want to raise the limit, submit an <https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&limitType=service-code-sns SNS Limit Increase case> . For __New limit value__ , enter your desired monthly spend limit. In the __Use Case Description__ field, explain that you are requesting an SMS monthly spend limit increase.
-- @DeliveryStatusIAMRole@ – The ARN of the IAM role that allows Amazon SNS to write logs about SMS deliveries in CloudWatch Logs. For each SMS message that you send, Amazon SNS writes a log that includes the message price, the success or failure status, the reason for failure (if the message failed), the message dwell time, and other information.
-- @DeliveryStatusSuccessSamplingRate@ – The percentage of successful SMS deliveries for which Amazon SNS will write logs in CloudWatch Logs. The value can be an integer from 0 - 100. For example, to write logs only for failed deliveries, set this value to @0@ . To write logs for 10% of your successful deliveries, set it to @10@ .
-- @DefaultSenderID@ – A string, such as your business brand, that is displayed as the sender on the receiving device. Support for sender IDs varies by country. The sender ID can be 1 - 11 alphanumeric characters, and it must contain at least one letter.
-- @DefaultSMSType@ – The type of SMS message that you will send by default. You can assign the following values:
--
--     * @Promotional@ – (Default) Noncritical messages, such as marketing messages. Amazon SNS optimizes the message delivery to incur the lowest cost.
--
--
--     * @Transactional@ – Critical messages that support customer transactions, such as one-time passcodes for multi-factor authentication. Amazon SNS optimizes the message delivery to achieve the highest reliability.
--
--
-- @UsageReportS3Bucket@ – The name of the Amazon S3 bucket to receive daily SMS usage reports from Amazon SNS. Each day, Amazon SNS will deliver a usage report as a CSV file to the bucket. The report includes the following information for each SMS message that was successfully delivered by your account:
--
--     * Time that the message was published (in UTC)
--
--
--     * Message ID
--
--
--     * Destination phone number
--
--
--     * Message type
--
--
--     * Delivery status
--
--
--     * Message price (in USD)
--
--
--     * Part number (a message is split into multiple parts if it is too long for a single message)
--
--
--     * Total number of parts
--
--
-- To receive the report, the bucket must have a policy that allows the Amazon SNS service principle to perform the @s3:PutObject@ and @s3:GetBucketLocation@ actions.
-- For an example bucket policy and usage report, see <https://docs.aws.amazon.com/sns/latest/dg/sms_stats.html Monitoring SMS Activity> in the /Amazon SNS Developer Guide/ .
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmsaAttributes :: Lens.Lens' SetSMSAttributes (Core.HashMap Types.String Types.String)
ssmsaAttributes = Lens.field @"attributes"
{-# DEPRECATED ssmsaAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Core.AWSRequest SetSMSAttributes where
  type Rs SetSMSAttributes = SetSMSAttributesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "SetSMSAttributes")
                Core.<> (Core.pure ("Version", "2010-03-31"))
                Core.<> ( Core.toQueryValue
                            "attributes"
                            (Core.toQueryMap "entry" "key" "value" attributes)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "SetSMSAttributesResult"
      ( \s h x ->
          SetSMSAttributesResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | The response for the SetSMSAttributes action.
--
-- /See:/ 'mkSetSMSAttributesResponse' smart constructor.
newtype SetSMSAttributesResponse = SetSMSAttributesResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetSMSAttributesResponse' value with any optional fields omitted.
mkSetSMSAttributesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SetSMSAttributesResponse
mkSetSMSAttributesResponse responseStatus =
  SetSMSAttributesResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmsarrsResponseStatus :: Lens.Lens' SetSMSAttributesResponse Core.Int
ssmsarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ssmsarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

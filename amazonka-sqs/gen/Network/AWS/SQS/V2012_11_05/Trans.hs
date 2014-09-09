{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Network.AWS.SQS.V2012_11_05.Trans
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Simple Queue Service (SQS) is a fast, reliable, scalable, fully
-- managed message queuing service. SQS makes it simple and cost-effective to
-- decouple the components of a cloud application. You can use SQS to transmit
-- any volume of data, at any level of throughput, without losing messages or
-- requiring other services to be always available. With SQS, you can offload
-- the administrative burden of operating and scaling a highly available
-- messaging cluster, while paying a low price for only what you use.
--
-- The 'State' operator variants from 'Control.Lens' such as '.=' can be
-- used to modify any additional request parameters before sending.
module Network.AWS.SQS.V2012_11_05.Trans
    (
    -- * AddPermission
      addPermission
    -- * ChangeMessageVisibility
    , changeMessageVisibility
    -- * ChangeMessageVisibilityBatch
    , changeMessageVisibilityBatch
    -- * CreateQueue
    , createQueue
    -- * DeleteMessage
    , deleteMessage
    -- * DeleteMessageBatch
    , deleteMessageBatch
    -- * DeleteQueue
    , deleteQueue
    -- * GetQueueAttributes
    , getQueueAttributes
    -- * GetQueueUrl
    , getQueueUrl
    -- * ListDeadLetterSourceQueues
    , listDeadLetterSourceQueues
    -- * ListQueues
    , listQueues
    -- * ReceiveMessage
    , receiveMessage
    -- * RemovePermission
    , removePermission
    -- * SendMessage
    , sendMessage
    -- * SendMessageBatch
    , sendMessageBatch
    -- * SetQueueAttributes
    , setQueueAttributes

    -- * Re-exported
    , module Control.Monad.Trans.AWS
    , module Network.AWS.SQS.V2012_11_05
    -- ** Lenses
    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.SQS.V2012_11_05

-- | Adds a permission to a queue for a specific principal. This allows for
-- sharing access to the queue. When you create a queue, you have full control
-- access rights for the queue. Only you (as owner of the queue) can grant or
-- deny permissions to the queue. For more information about these
-- permissions, see Shared Queues in the Amazon SQS Developer Guide.
-- AddPermission writes an Amazon SQS-generated policy. If you want to write
-- your own policy, use SetQueueAttributes to upload your policy. For more
-- information about writing your own policy, see Using The Access Policy
-- Language in the Amazon SQS Developer Guide. Some API actions take lists of
-- parameters. These lists are specified using the param.n notation. Values of
-- n are integers starting from 1. For example, a parameter list with two
-- elements looks like this: &amp;Attribute.1=this &amp;Attribute.2=that The
-- following example Query request grants a SendMessage permission to the
-- principal whose AWS account number is 125074342641.
-- http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=AddPermission &Label=testLabel &AWSAccountId.1=125074342641
-- &ActionName.1=SendMessage &AWSAccountId.2=125074342642
-- &ActionName.2=ReceiveMessage &Version=2009-02-01
-- &SignatureMethod=HmacSHA256 &Expires=2009-04-18T22%3A52%3A43PST
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &SignatureVersion=2
-- &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- 9a285199-c8d6-47c2-bdb2-314cb47d599d.
--
-- See: 'Network.AWS.SQS.V2012_11_05.AddPermission'
addPermission :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 , AWSRequest a
                 )
              => Text -- ^ 'apQueueUrl'
              -> Text -- ^ 'apLabel'
              -> [Text] -- ^ 'apAWSAccountIds'
              -> [Text] -- ^ 'apActions'
              -> State AddPermission a
              -> m AddPermissionResponse
addPermission p1 p2 p3 p4 s =
    send $ (mkAddPermission p1 p2 p3 p4) &~ s

-- | Changes the visibility timeout of a specified message in a queue to a new
-- value. The maximum allowed timeout value you can set the value to is 12
-- hours. This means you can't extend the timeout of a message in an existing
-- queue to more than a total visibility timeout of 12 hours. (For more
-- information visibility timeout, see Visibility Timeout in the Amazon SQS
-- Developer Guide.) For example, let's say you have a message and its default
-- message visibility timeout is 30 minutes. You could call
-- ChangeMessageVisiblity with a value of two hours and the effective timeout
-- would be two hours and 30 minutes. When that time comes near you could
-- again extend the time out by calling ChangeMessageVisiblity, but this time
-- the maximum allowed timeout would be 9 hours and 30 minutes. There is a
-- 120,000 limit for the number of inflight messages per queue. Messages are
-- inflight after they have been received from the queue by a consuming
-- component, but have not yet been deleted from the queue. If you reach the
-- 120,000 limit, you will receive an OverLimit error message from Amazon SQS.
-- To help avoid reaching the limit, you should delete the messages from the
-- queue after they have been processed. You can also increase the number of
-- queues you use to process the messages. If you attempt to set the
-- VisibilityTimeout to an amount more than the maximum time left, Amazon SQS
-- returns an error. It will not automatically recalculate and increase the
-- timeout to the maximum time remaining. Unlike with a queue, when you change
-- the visibility timeout for a specific message, that timeout value is
-- applied immediately but is not saved in memory for that message. If you
-- don't delete a message after it is received, the visibility timeout for the
-- message the next time it is received reverts to the original timeout value,
-- not the value you set with the ChangeMessageVisibility action. The
-- following example Query request changes the visibility timeout for a
-- message to 60 seconds.
-- http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=ChangeMessageVisibility &VisibilityTimeout=60
-- &ReceiptHandle=MbZj6wDWli%2BJvwwJaBV%2B3dcjk2YW2vA3%2BSTFFljT
-- M8tJJg6HRG6PYSasuWXPJB%2BCwLj1FjgXUv1uSj1gUPAWV66FU/WeR4mq2OKpEGY
-- WbnLmpRCJVAyeMjeU5ZBdtcQ%2BQEauMZc8ZRv37sIW2iJKq3M9MFx1YvV11A2x/K SbkJ0=
-- &Version=2009-02-01 &SignatureMethod=HmacSHA256
-- &Expires=2009-04-18T22%3A52%3A43PST &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &SignatureVersion=2 &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- 6a7a282a-d013-4a59-aba9-335b0fa48bed.
--
-- See: 'Network.AWS.SQS.V2012_11_05.ChangeMessageVisibility'
changeMessageVisibility :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           , AWSRequest a
                           )
                        => Text -- ^ 'cmvQueueUrl'
                        -> Text -- ^ 'cmvReceiptHandle'
                        -> Integer -- ^ 'cmvVisibilityTimeout'
                        -> State ChangeMessageVisibility a
                        -> m ChangeMessageVisibilityResponse
changeMessageVisibility p1 p2 p3 s =
    send $ (mkChangeMessageVisibility p1 p2 p3) &~ s

-- | Changes the visibility timeout of multiple messages. This is a batch
-- version of ChangeMessageVisibility. The result of the action on each
-- message is reported individually in the response. You can send up to 10
-- ChangeMessageVisibility requests with each ChangeMessageVisibilityBatch
-- action. Because the batch request can result in a combination of successful
-- and unsuccessful actions, you should check for batch errors even when the
-- call returns an HTTP status code of 200. Some API actions take lists of
-- parameters. These lists are specified using the param.n notation. Values of
-- n are integers starting from 1. For example, a parameter list with two
-- elements looks like this: &amp;Attribute.1=this &amp;Attribute.2=that
-- ChangeMessageVisibilityBatch request changes the visibility timeout
-- settings for two messages. You must URL encode the entire URL; however,
-- we've URL encoded only the message body to make the example easier for you
-- to read. http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- &Action=ChangeMessageVisibilityBatch &Version=2011-10-01
-- &ChangeMessageVisibilityBatchRequestEntry.1.Id=change_visibility_msg_2
-- &ChangeMessageVisibilityBatchRequestEntry.1.ReceiptHandle=gfk0T0R0waama4fVFffkjKzmhMCymjQvfTFk2LxT33G4ms5subrE0deLKWSscPU1oD3J9zgeS4PQQ3U30qOumIE6AdAv3w%2F%2Fa1IXW6AqaWhGsEPaLm3Vf6IiWqdM8u5imB%2BNTwj3tQRzOWdTOePjOjPcTpRxBtXix%2BEvwJOZUma9wabv%2BSw6ZHjwmNcVDx8dZXJhVp16Bksiox%2FGrUvrVTCJRTWTLc59oHLLF8sEkKzRmGNzTDGTiV%2BYjHfQj60FD3rVaXmzTsoNxRhKJ72uIHVMGVQiAGgBX6HGv9LDmYhPXw4hy%2FNgIg%3D%3D
-- &ChangeMessageVisibilityBatchRequestEntry.1.VisibilityTimeout=45
-- &ChangeMessageVisibilityBatchRequestEntry.2.Id=change_visibility_msg_3
-- &ChangeMessageVisibilityBatchRequestEntry.2.ReceiptHandle=gfk0T0R0waama4fVFffkjKzmhMCymjQvfTFk2LxT33FUgBz3%2BnougdeLKWSscPU1%2FXgx%2BxcNnjnQQ3U30qOumIE6AdAv3w%2F%2Fa1IXW6AqaWhGsEPaLm3Vf6IiWqdM8u5imB%2BNTwj3tQRzOWdTOePjOsogjZM%2F7kzn4Ew27XLU9I%2FYaWYmKvDbq%2Fk3HKVB9HfB43kE49atP2aWrzNL4yunG41Q4cfRRtfJdcGQGNHQ2%2Byd0Usf5qR1dZr1iDo5xk946eQat83AxTRP%2BY4Qi0V7FAeSLH9su9xpX6HGv9LDmYhPXw4hy%2FNgIg%3D%3D
-- &ChangeMessageVisibilityBatchRequestEntry.2.VisibilityTimeout=45
-- &SignatureMethod=HmacSHA256 &Expires=2011-10-18T22%3A52%3A43PST
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &SignatureVersion=2
-- &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE change_visibility_msg_2
-- change_visibility_msg_3 ca9668f7-ab1b-4f7a-8859-f15747ab17a7.
--
-- See: 'Network.AWS.SQS.V2012_11_05.ChangeMessageVisibilityBatch'
changeMessageVisibilityBatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadError AWS.Error m
                                , MonadReader Env m
                                , AWSRequest a
                                )
                             => Text -- ^ 'cmvbQueueUrl'
                             -> [ChangeMessageVisibilityBatchRequestEntry] -- ^ 'cmvbEntries'
                             -> State ChangeMessageVisibilityBatch a
                             -> m ChangeMessageVisibilityBatchResponse
changeMessageVisibilityBatch p1 p2 s =
    send $ (mkChangeMessageVisibilityBatch p1 p2) &~ s

-- | Creates a new queue, or returns the URL of an existing one. When you
-- request CreateQueue, you provide a name for the queue. To successfully
-- create a new queue, you must provide a name that is unique within the scope
-- of your own queues. If you delete a queue, you must wait at least 60
-- seconds before creating a queue with the same name. You may pass one or
-- more attributes in the request. If you do not provide a value for any
-- attribute, the queue will have the default value for that attribute.
-- Permitted attributes are the same that can be set using SetQueueAttributes.
-- Use GetQueueUrl to get a queue's URL. GetQueueUrl requires only the
-- QueueName parameter. If you provide the name of an existing queue, along
-- with the exact names and values of all the queue's attributes, CreateQueue
-- returns the queue URL for the existing queue. If the queue name, attribute
-- names, or attribute values do not match an existing queue, CreateQueue
-- returns an error. Some API actions take lists of parameters. These lists
-- are specified using the param.n notation. Values of n are integers starting
-- from 1. For example, a parameter list with two elements looks like this:
-- &amp;Attribute.1=this &amp;Attribute.2=that The following example Query
-- request creates a new queue named testQueue.
-- http://sqs.us-east-1.amazonaws.com/ ?Action=CreateQueue
-- &QueueName=testQueue &Attribute.1.Name=VisibilityTimeout
-- &Attribute.1.Value=40 &Version=2011-10-01 &SignatureMethod=HmacSHA256
-- &Expires=2011-10-18T22%3A52%3A43PST &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &SignatureVersion=2 &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- http://&useast1-query;/123456789012/testQueue
-- 7a62c49f-347e-4fc4-9331-6e8e7a96aa73.
--
-- See: 'Network.AWS.SQS.V2012_11_05.CreateQueue'
createQueue :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               , AWSRequest a
               )
            => Text -- ^ 'cqQueueName'
            -> State CreateQueue a
            -> m CreateQueueResponse
createQueue p1 s =
    send $ (mkCreateQueue p1) &~ s

-- | Deletes the specified message from the specified queue. You specify the
-- message by using the message's receipt handle and not the message ID you
-- received when you sent the message. Even if the message is locked by
-- another reader due to the visibility timeout setting, it is still deleted
-- from the queue. If you leave a message in the queue for longer than the
-- queue's configured retention period, Amazon SQS automatically deletes it.
-- The receipt handle is associated with a specific instance of receiving the
-- message. If you receive a message more than once, the receipt handle you
-- get each time you receive the message is different. When you request
-- DeleteMessage, if you don't provide the most recently received receipt
-- handle for the message, the request will still succeed, but the message
-- might not be deleted. It is possible you will receive a message even after
-- you have deleted it. This might happen on rare occasions if one of the
-- servers storing a copy of the message is unavailable when you request to
-- delete the message. The copy remains on the server and might be returned to
-- you again on a subsequent receive request. You should create your system to
-- be idempotent so that receiving a particular message more than once is not
-- a problem. The following example Query request deletes a message from the
-- queue named testQueue.
-- http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=DeleteMessage
-- &ReceiptHandle=MbZj6wDWli%2BJvwwJaBV%2B3dcjk2YW2vA3%2BSTFFljT
-- M8tJJg6HRG6PYSasuWXPJB%2BCwLj1FjgXUv1uSj1gUPAWV66FU/WeR4mq2OKpEGY
-- WbnLmpRCJVAyeMjeU5ZBdtcQ%2BQEauMZc8ZRv37sIW2iJKq3M9MFx1YvV11A2x/K SbkJ0=
-- &Version=2009-02-01 &SignatureMethod=HmacSHA256
-- &Expires=2009-04-18T22%3A52%3A43PST &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &SignatureVersion=2 &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- b5293cb5-d306-4a17-9048-b263635abe42.
--
-- See: 'Network.AWS.SQS.V2012_11_05.DeleteMessage'
deleteMessage :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 , AWSRequest a
                 )
              => Text -- ^ 'dmQueueUrl'
              -> Text -- ^ 'dmReceiptHandle'
              -> State DeleteMessage a
              -> m DeleteMessageResponse
deleteMessage p1 p2 s =
    send $ (mkDeleteMessage p1 p2) &~ s

-- | Deletes multiple messages. This is a batch version of DeleteMessage. The
-- result of the delete action on each message is reported individually in the
-- response. Because the batch request can result in a combination of
-- successful and unsuccessful actions, you should check for batch errors even
-- when the call returns an HTTP status code of 200. Some API actions take
-- lists of parameters. These lists are specified using the param.n notation.
-- Values of n are integers starting from 1. For example, a parameter list
-- with two elements looks like this: &amp;Attribute.1=this
-- &amp;Attribute.2=that The following example DeleteMessageBatch request
-- deletes two messages. You must URL encode the entire URL; however, we've
-- URL encoded only the message body to make the example easier for you to
-- read. http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- &Action=DeleteMessageBatch &Version=2011-10-01
-- &DeleteMessageBatchRequestEntry.1.Id=msg1
-- &DeleteMessageBatchRequestEntry.1.ReceiptHandle=gfk0T0R0waama4fVFffkjPQrrvzMrOg0fTFk2LxT33EuB8wR0ZCFgKWyXGWFoqqpCIiprQUEhir%2F5LeGPpYTLzjqLQxyQYaQALeSNHb0us3uE84uujxpBhsDkZUQkjFFkNqBXn48xlMcVhTcI3YLH%2Bd%2BIqetIOHgBCZAPx6r%2B09dWaBXei6nbK5Ygih21DCDdAwFV68Jo8DXhb3ErEfoDqx7vyvC5nCpdwqv%2BJhU%2FTNGjNN8t51v5c%2FAXvQsAzyZVNapxUrHIt4NxRhKJ72uICcxruyE8eRXlxIVNgeNP8ZEDcw7zZU1Zw%3D%3D
-- &DeleteMessageBatchRequestEntry.2.Id=msg2
-- &DeleteMessageBatchRequestEntry.2.ReceiptHandle=gfk0T0R0waama4fVFffkjKzmhMCymjQvfTFk2LxT33G4ms5subrE0deLKWSscPU1oD3J9zgeS4PQQ3U30qOumIE6AdAv3w%2F%2Fa1IXW6AqaWhGsEPaLm3Vf6IiWqdM8u5imB%2BNTwj3tQRzOWdTOePjOjPcTpRxBtXix%2BEvwJOZUma9wabv%2BSw6ZHjwmNcVDx8dZXJhVp16Bksiox%2FGrUvrVTCJRTWTLc59oHLLF8sEkKzRmGNzTDGTiV%2BYjHfQj60FD3rVaXmzTsoNxRhKJ72uIHVMGVQiAGgB%2BqAbSqfKHDQtVOmJJgkHug%3D%3D
-- &SignatureMethod=HmacSHA256 &Expires=2011-10-18T22%3A52%3A43PST
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &SignatureVersion=2
-- &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE msg1 msg2
-- d6f86b7a-74d1-4439-b43f-196a1e29cd85.
--
-- See: 'Network.AWS.SQS.V2012_11_05.DeleteMessageBatch'
deleteMessageBatch :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      , AWSRequest a
                      )
                   => Text -- ^ 'dmbQueueUrl'
                   -> [DeleteMessageBatchRequestEntry] -- ^ 'dmbEntries'
                   -> State DeleteMessageBatch a
                   -> m DeleteMessageBatchResponse
deleteMessageBatch p1 p2 s =
    send $ (mkDeleteMessageBatch p1 p2) &~ s

-- | Deletes the queue specified by the queue URL, regardless of whether the
-- queue is empty. If the specified queue does not exist, Amazon SQS returns a
-- successful response. Use DeleteQueue with care; once you delete your queue,
-- any messages in the queue are no longer available. When you delete a queue,
-- the deletion process takes up to 60 seconds. Requests you send involving
-- that queue during the 60 seconds might succeed. For example, a SendMessage
-- request might succeed, but after the 60 seconds, the queue and that message
-- you sent no longer exist. Also, when you delete a queue, you must wait at
-- least 60 seconds before creating a queue with the same name. We reserve the
-- right to delete queues that have had no activity for more than 30 days. For
-- more information, see How Amazon SQS Queues Work in the Amazon SQS
-- Developer Guide. The following example Query request deletes the specified
-- queue. http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=DeleteQueue &Version=2009-02-01 &SignatureMethod=HmacSHA256
-- &Expires=2009-04-18T22%3A52%3A43PST &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &SignatureVersion=2 &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- 6fde8d1e-52cd-4581-8cd9-c512f4c64223.
--
-- See: 'Network.AWS.SQS.V2012_11_05.DeleteQueue'
deleteQueue :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               , AWSRequest a
               )
            => Text -- ^ 'dqQueueUrl'
            -> State DeleteQueue a
            -> m DeleteQueueResponse
deleteQueue p1 s =
    send $ (mkDeleteQueue p1) &~ s

-- | Gets attributes for the specified queue. The following attributes are
-- supported: All - returns all values. ApproximateNumberOfMessages - returns
-- the approximate number of visible messages in a queue. For more
-- information, see Resources Required to Process Messages in the Amazon SQS
-- Developer Guide. ApproximateNumberOfMessagesNotVisible - returns the
-- approximate number of messages that are not timed-out and not deleted. For
-- more information, see Resources Required to Process Messages in the Amazon
-- SQS Developer Guide. VisibilityTimeout - returns the visibility timeout for
-- the queue. For more information about visibility timeout, see Visibility
-- Timeout in the Amazon SQS Developer Guide. CreatedTimestamp - returns the
-- time when the queue was created (epoch time in seconds).
-- LastModifiedTimestamp - returns the time when the queue was last changed
-- (epoch time in seconds). Policy - returns the queue's policy.
-- MaximumMessageSize - returns the limit of how many bytes a message can
-- contain before Amazon SQS rejects it. MessageRetentionPeriod - returns the
-- number of seconds Amazon SQS retains a message. QueueArn - returns the
-- queue's Amazon resource name (ARN). ApproximateNumberOfMessagesDelayed -
-- returns the approximate number of messages that are pending to be added to
-- the queue. DelaySeconds - returns the default delay on the queue in
-- seconds. ReceiveMessageWaitTimeSeconds - returns the time for which a
-- ReceiveMessage call will wait for a message to arrive. RedrivePolicy -
-- returns the parameters for dead letter queue functionality of the source
-- queue. For more information about RedrivePolicy and dead letter queues, see
-- Using Amazon SQS Dead Letter Queues in the Amazon SQS Developer Guide.
-- Going forward, new attributes might be added. If you are writing code that
-- calls this action, we recommend that you structure your code so that it can
-- handle new attributes gracefully. Some API actions take lists of
-- parameters. These lists are specified using the param.n notation. Values of
-- n are integers starting from 1. For example, a parameter list with two
-- elements looks like this: &amp;Attribute.1=this &amp;Attribute.2=that The
-- following example Query requests gets all the attribute values for the
-- specified queue. http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=GetQueueAttributes &AttributeName.1=All &Version=2012-11-05
-- &SignatureMethod=HmacSHA256 &Expires=2013-10-18T22%3A52%3A43PST
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &SignatureVersion=2
-- &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE ReceiveMessageWaitTimeSeconds
-- 2 VisibilityTimeout 30 ApproximateNumberOfMessages 0
-- ApproximateNumberOfMessagesNotVisible 0 CreatedTimestamp 1286771522
-- LastModifiedTimestamp 1286771522 QueueArn
-- arn:aws:sqs:us-east-1:123456789012:qfoo MaximumMessageSize 8192
-- MessageRetentionPeriod 345600 1ea71be5-b5a2-4f9d-b85a-945d8d08cd0b The
-- following example Query request gets three attribute values for the
-- specified queue. http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=GetQueueAttributes &Action=GetQueueAttributes &Version=2012-11-05
-- &AttributeName.1=VisibilityTimeout &AttributeName.2=DelaySeconds
-- &AttributeName.3=ReceiveMessageWaitTimeSeconds &SignatureMethod=HmacSHA256
-- &Expires=2013-10-18T22%3A52%3A43PST &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &SignatureVersion=2 &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- VisibilityTimeout 30 DelaySeconds 0 ReceiveMessageWaitTimeSeconds 2.
--
-- See: 'Network.AWS.SQS.V2012_11_05.GetQueueAttributes'
getQueueAttributes :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      , AWSRequest a
                      )
                   => Text -- ^ 'gqaQueueUrl'
                   -> State GetQueueAttributes a
                   -> m GetQueueAttributesResponse
getQueueAttributes p1 s =
    send $ (mkGetQueueAttributes p1) &~ s

-- | Returns the URL of an existing queue. This action provides a simple way to
-- retrieve the URL of an Amazon SQS queue. To access a queue that belongs to
-- another AWS account, use the QueueOwnerAWSAccountId parameter to specify
-- the account ID of the queue's owner. The queue's owner must grant you
-- permission to access the queue. For more information about shared queue
-- access, see AddPermission or go to Shared Queues in the Amazon SQS
-- Developer Guide. The following example Query request gets the URL for the
-- specified queue. http://sqs.us-east-1.amazonaws.com/ ?Action=GetQueueUrl
-- &QueueName=testQueue &Version=2011-10-01 &SignatureMethod=HmacSHA256
-- &Expires=2011-10-24T22%3A52%3A43PST &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &SignatureVersion=2 &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- http://&useast1-query;/123456789012/testQueue
-- 470a6f13-2ed9-4181-ad8a-2fdea142988e.
--
-- See: 'Network.AWS.SQS.V2012_11_05.GetQueueUrl'
getQueueUrl :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               , AWSRequest a
               )
            => Text -- ^ 'gquQueueName'
            -> State GetQueueUrl a
            -> m GetQueueUrlResponse
getQueueUrl p1 s =
    send $ (mkGetQueueUrl p1) &~ s

-- | Returns a list of your queues that have the RedrivePolicy queue attribute
-- configured with a dead letter queue. The following example Query request
-- returns a list of dead letter source queues. In this example only one
-- source queue, MySourceQueue, was configured with a dead letter queue.
-- Action=ListDeadLetterSourceQueues &Version=2012-11-05
-- http://sqs.us-east-1.amazonaws.com/123456789012/MySourceQueue
-- 8ffb921f-b85e-53d9-abcf-d8d0057f38fc For more information about using dead
-- letter queues, see Using Amazon SQS Dead Letter Queues.
--
-- See: 'Network.AWS.SQS.V2012_11_05.ListDeadLetterSourceQueues'
listDeadLetterSourceQueues :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env m
                              , AWSRequest a
                              )
                           => Text -- ^ 'ldlsqQueueUrl'
                           -> State ListDeadLetterSourceQueues a
                           -> m ListDeadLetterSourceQueuesResponse
listDeadLetterSourceQueues p1 s =
    send $ (mkListDeadLetterSourceQueues p1) &~ s

-- | Returns a list of your queues. The maximum number of queues that can be
-- returned is 1000. If you specify a value for the optional QueueNamePrefix
-- parameter, only queues with a name beginning with the specified value are
-- returned. The following example Query request returns the queues whose
-- names begin with the letter "T". http://sqs.us-east-1.amazonaws.com/
-- ?Action=ListQueues &QueueNamePrefix=t &Version=2009-02-01
-- &SignatureMethod=HmacSHA256 &Expires=2009-04-18T22%3A52%3A43PST
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &SignatureVersion=2
-- &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- http://sqs.us-east-1.amazonaws.com/123456789012/testQueue
-- 725275ae-0b9b-4762-b238-436d7c65a1ac.
--
-- See: 'Network.AWS.SQS.V2012_11_05.ListQueues'
listQueues :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env m
              , AWSRequest a
              )
           => State ListQueues a
           -> m ListQueuesResponse
listQueues s =
    send (mkListQueues &~ s)

-- | Retrieves one or more messages, with a maximum limit of 10 messages, from
-- the specified queue. Long poll support is enabled by using the
-- WaitTimeSeconds parameter. For more information, see Amazon SQS Long Poll
-- in the Amazon SQS Developer Guide. Short poll is the default behavior where
-- a weighted random set of machines is sampled on a ReceiveMessage call. This
-- means only the messages on the sampled machines are returned. If the number
-- of messages in the queue is small (less than 1000), it is likely you will
-- get fewer messages than you requested per ReceiveMessage call. If the
-- number of messages in the queue is extremely small, you might not receive
-- any messages in a particular ReceiveMessage response; in which case you
-- should repeat the request. For each message returned, the response includes
-- the following: Message body MD5 digest of the message body. For information
-- about MD5, go to http://www.faqs.org/rfcs/rfc1321.html. Message ID you
-- received when you sent the message to the queue. Receipt handle. Message
-- attributes. MD5 digest of the message attributes. The receipt handle is the
-- identifier you must provide when deleting the message. For more
-- information, see Queue and Message Identifiers in the Amazon SQS Developer
-- Guide. You can provide the VisibilityTimeout parameter in your request,
-- which will be applied to the messages that Amazon SQS returns in the
-- response. If you do not include the parameter, the overall visibility
-- timeout for the queue is used for the returned messages. For more
-- information, see Visibility Timeout in the Amazon SQS Developer Guide.
-- Going forward, new attributes might be added. If you are writing code that
-- calls this action, we recommend that you structure your code so that it can
-- handle new attributes gracefully. The following example Query request
-- receives messages from the specified queue.
-- http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=ReceiveMessage &MaxNumberOfMessages=5 &VisibilityTimeout=15
-- &AttributeName=All; &Version=2009-02-01 &SignatureMethod=HmacSHA256
-- &Expires=2009-04-18T22%3A52%3A43PST &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &SignatureVersion=2 &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- 5fea7756-0ea4-451a-a703-a558b933e274
-- MbZj6wDWli+JvwwJaBV+3dcjk2YW2vA3+STFFljTM8tJJg6HRG6PYSasuWXPJB+Cw
-- Lj1FjgXUv1uSj1gUPAWV66FU/WeR4mq2OKpEGYWbnLmpRCJVAyeMjeU5ZBdtcQ+QE
-- auMZc8ZRv37sIW2iJKq3M9MFx1YvV11A2x/KSbkJ0= fafb00f5732ab283681e124bf8747ed1
-- This is a test message SenderId 195004372649 SentTimestamp 1238099229000
-- ApproximateReceiveCount 5 ApproximateFirstReceiveTimestamp 1250700979248
-- b6633655-283d-45b4-aee4-4e84e0ae6afa.
--
-- See: 'Network.AWS.SQS.V2012_11_05.ReceiveMessage'
receiveMessage :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  , AWSRequest a
                  )
               => Text -- ^ 'rmQueueUrl'
               -> State ReceiveMessage a
               -> m ReceiveMessageResponse
receiveMessage p1 s =
    send $ (mkReceiveMessage p1) &~ s

-- | Revokes any permissions in the queue policy that matches the specified
-- Label parameter. Only the owner of the queue can remove permissions. The
-- following example Query request removes the testLabel permission on the
-- queue named testQueue.
-- http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=RemovePermission &Label=testLabel &Version=2009-02-01
-- &SignatureMethod=HmacSHA256 &Expires=2009-04-18T22%3A52%3A43PST
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &SignatureVersion=2
-- &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- f8bdb362-6616-42c0-977a-ce9a8bcce3bb.
--
-- See: 'Network.AWS.SQS.V2012_11_05.RemovePermission'
removePermission :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    , AWSRequest a
                    )
                 => Text -- ^ 'rpQueueUrl'
                 -> Text -- ^ 'rpLabel'
                 -> State RemovePermission a
                 -> m RemovePermissionResponse
removePermission p1 p2 s =
    send $ (mkRemovePermission p1 p2) &~ s

-- | Delivers a message to the specified queue. With Amazon SQS, you now have
-- the ability to send large payload messages that are up to 256KB (262,144
-- bytes) in size. To send large payloads, you must use an AWS SDK that
-- supports SigV4 signing. To verify whether SigV4 is supported for an AWS
-- SDK, check the SDK release notes. The following list shows the characters
-- (in Unicode) allowed in your message, according to the W3C XML
-- specification. For more information, go to
-- http://www.w3.org/TR/REC-xml/#charsets If you send any characters not
-- included in the list, your request will be rejected. #x9 | #xA | #xD |
-- [#x20 to #xD7FF] | [#xE000 to #xFFFD] | [#x10000 to #x10FFFF] The following
-- example SendMessage request sends a message containing "This is a test
-- message" to the queue. You must URL encode the entire URL; however, we've
-- URL encoded only the message body to make the example easier for you to
-- read. The following example response includes the MD5 digest for "This is a
-- test message". http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=SendMessage &MessageBody=This+is+a+test+message
-- &MessageAttribute.1.Name=test_attribute_name_1
-- &MessageAttribute.1.Value.StringValue=test_attribute_value_1
-- &MessageAttribute.1.Value.DataType=String
-- &MessageAttribute.2.Name=test_attribute_name_2
-- &MessageAttribute.2.Value.StringValue=test_attribute_value_2
-- &MessageAttribute.2.Value.DataType=String &Version=2012-11-05
-- &SignatureMethod=HmacSHA256 &Expires=2014-05-05T22%3A52%3A43PST
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &SignatureVersion=2
-- &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- fafb00f5732ab283681e124bf8747ed1 3ae8f24a165a8cedc005670c81a27295
-- 5fea7756-0ea4-451a-a703-a558b933e274 27daac76-34dd-47df-bd01-1f6e873584a0.
--
-- See: 'Network.AWS.SQS.V2012_11_05.SendMessage'
sendMessage :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               , AWSRequest a
               )
            => Text -- ^ 'smQueueUrl'
            -> Text -- ^ 'smMessageBody'
            -> State SendMessage a
            -> m SendMessageResponse
sendMessage p1 p2 s =
    send $ (mkSendMessage p1 p2) &~ s

-- | Delivers up to ten messages to the specified queue. This is a batch version
-- of SendMessage. The result of the send action on each message is reported
-- individually in the response. The maximum allowed individual message size
-- is 256 KB (262,144 bytes). The maximum total payload size (i.e., the sum of
-- all a batch's individual message lengths) is also 256 KB (262,144 bytes).
-- If the DelaySeconds parameter is not specified for an entry, the default
-- for the queue is used. The following list shows the characters (in Unicode)
-- that are allowed in your message, according to the W3C XML specification.
-- For more information, go to http://www.faqs.org/rfcs/rfc1321.html. If you
-- send any characters that are not included in the list, your request will be
-- rejected. #x9 | #xA | #xD | [#x20 to #xD7FF] | [#xE000 to #xFFFD] |
-- [#x10000 to #x10FFFF] Because the batch request can result in a combination
-- of successful and unsuccessful actions, you should check for batch errors
-- even when the call returns an HTTP status code of 200. Some API actions
-- take lists of parameters. These lists are specified using the param.n
-- notation. Values of n are integers starting from 1. For example, a
-- parameter list with two elements looks like this: &amp;Attribute.1=this
-- &amp;Attribute.2=that The following example SendMessageBatch request sends
-- two messages to the queue. You must URL encode the entire URL; however,
-- we've URL encoded only the message body to make the example easier for you
-- to read. The following example response includes the MD5 digest for the
-- messages. http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=SendMessageBatch &SendMessageBatchRequestEntry.1.Id=test_msg_001
-- &SendMessageBatchRequestEntry.1.MessageBody=test%20message%20body%201
-- &SendMessageBatchRequestEntry.2.Id=test_msg_002
-- &SendMessageBatchRequestEntry.2.MessageBody=test%20message%20body%202
-- &SendMessageBatchRequestEntry.2.DelaySeconds=60
-- &SendMessageBatchRequestEntry.2.MessageAttribute.1.Name=test_attribute_name_1
-- 
-- &SendMessageBatchRequestEntry.2.MessageAttribute.1.Value.StringValue=test_attribute_value_1
-- &SendMessageBatchRequestEntry.2.MessageAttribute.1.Value.DataType=String
-- &Version=2012-11-05 &SignatureMethod=HmacSHA256
-- &Expires=2014-05-05T22%3A52%3A43PST &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &SignatureVersion=2 &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- test_msg_001 0a5231c7-8bff-4955-be2e-8dc7c50a25fa
-- 0e024d309850c78cba5eabbeff7cae71 test_msg_002
-- 15ee1ed3-87e7-40c1-bdaa-2e49968ea7e9 7fb8146a82f95e0af155278f406862c2
-- 295c5fa15a51aae6884d1d7c1d99ca50 ca1ad5d0-8271-408b-8d0f-1351bf547e74.
--
-- See: 'Network.AWS.SQS.V2012_11_05.SendMessageBatch'
sendMessageBatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    , AWSRequest a
                    )
                 => Text -- ^ 'smbQueueUrl'
                 -> [SendMessageBatchRequestEntry] -- ^ 'smbEntries'
                 -> State SendMessageBatch a
                 -> m SendMessageBatchResponse
sendMessageBatch p1 p2 s =
    send $ (mkSendMessageBatch p1 p2) &~ s

-- | Sets the value of one or more queue attributes. When you change a queue's
-- attributes, the change can take up to 60 seconds for most of the attributes
-- to propagate throughout the SQS system. Changes made to the
-- MessageRetentionPeriod attribute can take up to 15 minutes. Going forward,
-- new attributes might be added. If you are writing code that calls this
-- action, we recommend that you structure your code so that it can handle new
-- attributes gracefully. The following example Query request sets a policy
-- that gives all users ReceiveMessage permission for the queue named
-- testQueue. For more examples of policies, see Amazon SQS Policy Examples in
-- the Amazon SQS Developer Guide.
-- http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=SetQueueAttributes &Version=2009-02-01 &Attribute.Name=Policy
-- &Attribute.Value=%7B%22Version%22%3A%222008-10-17%22%2C%22Id%22
-- %3A%22%2F123456789012%2FtestQueue%2FSQSDefaultPolicy%22%2C%22Stat
-- ement%22%3A%5B%7B%22Sid%22%3A%22Queue1ReceiveMessage%22%2C%22Effe
-- ct%22%3A%22Allow%22%2C%22Principal%22%3A%7B%22AWS%22%3A%22*%22%7D
-- %2C%22Action%22%3A%22SQS%3AReceiveMessage%22%2C%22Resource%22%3A%
-- 22arn%3Aaws%3Aaws%3Asqs%3Aus%2Deast%2D1%3A123456789012%3AtestQueue%22%7D%5D%7D
-- &Timestamp=2009-05-06T16%3A57%3A31.000Z
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &Signature=%2Bd7ZlPIdnb%2BhpEna2TgfwQjfGF8%3D The following example Query
-- request sets the visibility timeout to 35 seconds for the queue named
-- testQueue. Note: There is a 120,000 limit for the number of inflight
-- messages per queue. Messages are inflight after they have been received by
-- the queue, but have not yet been deleted from the queue. If you reach the
-- 120,000 limit, you will receive an OverLimit error message from Amazon SQS.
-- To help avoid reaching the limit, you should delete the messages from the
-- queue after they have been processed. You can also increase the number of
-- queues you use to process the messages.
-- http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=SetQueueAttributes &Attribute.Name=VisibilityTimeout
-- &Attribute.Value=35 &Version=2009-02-01 &SignatureMethod=HmacSHA256
-- &Expires=2009-04-18T22%3A52%3A43PST &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &SignatureVersion=2 &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- e5cca473-4fc0-4198-a451-8abb94d02c75.
--
-- See: 'Network.AWS.SQS.V2012_11_05.SetQueueAttributes'
setQueueAttributes :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      , AWSRequest a
                      )
                   => Text -- ^ 'sqaQueueUrl'
                   -> Map QueueAttributeName Text -- ^ 'sqaAttributes'
                   -> State SetQueueAttributes a
                   -> m SetQueueAttributesResponse
setQueueAttributes p1 p2 s =
    send $ (mkSetQueueAttributes p1 p2) &~ s

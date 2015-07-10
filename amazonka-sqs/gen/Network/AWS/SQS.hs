{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Welcome to the /Amazon Simple Queue Service API Reference/. This section
-- describes who should read this guide, how the guide is organized, and
-- other resources related to the Amazon Simple Queue Service (Amazon SQS).
--
-- Amazon SQS offers reliable and scalable hosted queues for storing
-- messages as they travel between computers. By using Amazon SQS, you can
-- move data between distributed components of your applications that
-- perform different tasks without losing messages or requiring each
-- component to be always available.
--
-- Helpful Links:
--
-- -   <http://queue.amazonaws.com/doc/2012-11-05/QueueService.wsdl Current WSDL (2012-11-05)>
-- -   <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/MakingRequestsArticle.html Making API Requests>
-- -   <http://aws.amazon.com/sqs/ Amazon SQS product page>
-- -   <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSMessageAttributes.html Using Amazon SQS Message Attributes>
-- -   <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSDeadLetterQueue.html Using Amazon SQS Dead Letter Queues>
-- -   <http://docs.aws.amazon.com/general/latest/gr/rande.html#sqs_region Regions and Endpoints>
--
-- We also provide SDKs that enable you to access Amazon SQS from your
-- preferred programming language. The SDKs contain functionality that
-- automatically takes care of tasks such as:
--
-- -   Cryptographically signing your service requests
-- -   Retrying requests
-- -   Handling error responses
--
-- For a list of available SDKs, go to
-- <http://aws.amazon.com/tools/ Tools for Amazon Web Services>.
module Network.AWS.SQS
    ( module Export
    ) where

import           Network.AWS.SQS.AddPermission                as Export
import           Network.AWS.SQS.ChangeMessageVisibility      as Export
import           Network.AWS.SQS.ChangeMessageVisibilityBatch as Export
import           Network.AWS.SQS.CreateQueue                  as Export
import           Network.AWS.SQS.DeleteMessage                as Export
import           Network.AWS.SQS.DeleteMessageBatch           as Export
import           Network.AWS.SQS.DeleteQueue                  as Export
import           Network.AWS.SQS.GetQueueAttributes           as Export
import           Network.AWS.SQS.GetQueueURL                  as Export
import           Network.AWS.SQS.ListDeadLetterSourceQueues   as Export
import           Network.AWS.SQS.ListQueues                   as Export
import           Network.AWS.SQS.PurgeQueue                   as Export
import           Network.AWS.SQS.ReceiveMessage               as Export
import           Network.AWS.SQS.RemovePermission             as Export
import           Network.AWS.SQS.SendMessage                  as Export
import           Network.AWS.SQS.SendMessageBatch             as Export
import           Network.AWS.SQS.SetQueueAttributes           as Export
import           Network.AWS.SQS.Types                        as Export
import           Network.AWS.SQS.Waiters                      as Export

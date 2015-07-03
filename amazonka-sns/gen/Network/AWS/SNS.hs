-- Module      : Network.AWS.SNS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Amazon Simple Notification Service
--
-- Amazon Simple Notification Service (Amazon SNS) is a web service that
-- enables you to build distributed web-enabled applications. Applications
-- can use Amazon SNS to easily push real-time notification messages to
-- interested subscribers over multiple delivery protocols. For more
-- information about this product see
-- <http://aws.amazon.com/sns/ http:\/\/aws.amazon.com\/sns>. For detailed
-- information about Amazon SNS features and their associated API calls,
-- see the
-- <http://docs.aws.amazon.com/sns/latest/dg/ Amazon SNS Developer Guide>.
--
-- We also provide SDKs that enable you to access Amazon SNS from your
-- preferred programming language. The SDKs contain functionality that
-- automatically takes care of tasks such as: cryptographically signing
-- your service requests, retrying requests, and handling error responses.
-- For a list of available SDKs, go to
-- <http://aws.amazon.com/tools/ Tools for Amazon Web Services>.
module Network.AWS.SNS
    ( module Export
    ) where

import           Network.AWS.SNS.AddPermission                      as Export
import           Network.AWS.SNS.ConfirmSubscription                as Export
import           Network.AWS.SNS.CreatePlatformApplication          as Export
import           Network.AWS.SNS.CreatePlatformEndpoint             as Export
import           Network.AWS.SNS.CreateTopic                        as Export
import           Network.AWS.SNS.DeleteEndpoint                     as Export
import           Network.AWS.SNS.DeletePlatformApplication          as Export
import           Network.AWS.SNS.DeleteTopic                        as Export
import           Network.AWS.SNS.GetEndpointAttributes              as Export
import           Network.AWS.SNS.GetPlatformApplicationAttributes   as Export
import           Network.AWS.SNS.GetSubscriptionAttributes          as Export
import           Network.AWS.SNS.GetTopicAttributes                 as Export
import           Network.AWS.SNS.ListEndpointsByPlatformApplication as Export
import           Network.AWS.SNS.ListPlatformApplications           as Export
import           Network.AWS.SNS.ListSubscriptions                  as Export
import           Network.AWS.SNS.ListSubscriptionsByTopic           as Export
import           Network.AWS.SNS.ListTopics                         as Export
import           Network.AWS.SNS.Publish                            as Export
import           Network.AWS.SNS.RemovePermission                   as Export
import           Network.AWS.SNS.SetEndpointAttributes              as Export
import           Network.AWS.SNS.SetPlatformApplicationAttributes   as Export
import           Network.AWS.SNS.SetSubscriptionAttributes          as Export
import           Network.AWS.SNS.SetTopicAttributes                 as Export
import           Network.AWS.SNS.Subscribe                          as Export
import           Network.AWS.SNS.Types                              as Export
import           Network.AWS.SNS.Unsubscribe                        as Export
import           Network.AWS.SNS.Waiters                            as Export

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Subscribe
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Prepares to subscribe an endpoint by sending the endpoint a confirmation message. To actually create a subscription, the endpoint owner must call the @ConfirmSubscription@ action with the token from the confirmation message. Confirmation tokens are valid for three days.
--
--
-- This action is throttled at 100 transactions per second (TPS).
--
module Network.AWS.SNS.Subscribe
    (
    -- * Creating a Request
      subscribe
    , Subscribe
    -- * Request Lenses
    , subReturnSubscriptionARN
    , subAttributes
    , subEndpoint
    , subTopicARN
    , subProtocol

    -- * Destructuring the Response
    , subscribeResponse
    , SubscribeResponse
    -- * Response Lenses
    , srsSubscriptionARN
    , srsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | Input for Subscribe action.
--
--
--
-- /See:/ 'subscribe' smart constructor.
data Subscribe = Subscribe'
  { _subReturnSubscriptionARN :: !(Maybe Bool)
  , _subAttributes            :: !(Maybe (Map Text Text))
  , _subEndpoint              :: !(Maybe Text)
  , _subTopicARN              :: !Text
  , _subProtocol              :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Subscribe' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'subReturnSubscriptionARN' - Sets whether the response from the @Subscribe@ request includes the subscription ARN, even if the subscription is not yet confirmed. If you set this parameter to @false@ , the response includes the ARN for confirmed subscriptions, but it includes an ARN value of "pending subscription" for subscriptions that are not yet confirmed. A subscription becomes confirmed when the subscriber calls the @ConfirmSubscription@ action with a confirmation token. If you set this parameter to @true@ , the response includes the ARN in all cases, even if the subscription is not yet confirmed. The default value is @false@ .
--
-- * 'subAttributes' - A map of attributes with their corresponding values. The following lists the names, descriptions, and values of the special request parameters that the @SetTopicAttributes@ action uses:     * @DeliveryPolicy@

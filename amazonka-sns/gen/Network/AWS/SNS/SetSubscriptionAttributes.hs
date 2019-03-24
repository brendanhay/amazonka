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
-- Module      : Network.AWS.SNS.SetSubscriptionAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a subscription owner to set an attribute of the subscription to a new value.
--
--
module Network.AWS.SNS.SetSubscriptionAttributes
    (
    -- * Creating a Request
      setSubscriptionAttributes
    , SetSubscriptionAttributes
    -- * Request Lenses
    , ssaAttributeValue
    , ssaSubscriptionARN
    , ssaAttributeName

    -- * Destructuring the Response
    , setSubscriptionAttributesResponse
    , SetSubscriptionAttributesResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | Input for SetSubscriptionAttributes action.
--
--
--
-- /See:/ 'setSubscriptionAttributes' smart constructor.
data SetSubscriptionAttributes = SetSubscriptionAttributes'
  { _ssaAttributeValue  :: !(Maybe Text)
  , _ssaSubscriptionARN :: !Text
  , _ssaAttributeName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetSubscriptionAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssaAttributeValue' - The new value for the attribute in JSON format.
--
-- * 'ssaSubscriptionARN' - The ARN of the subscription to modify.
--
-- * 'ssaAttributeName' - A map of attributes with their corresponding values. The following lists the names, descriptions, and values of the special request parameters that the @SetTopicAttributes@ action uses:     * @DeliveryPolicy@

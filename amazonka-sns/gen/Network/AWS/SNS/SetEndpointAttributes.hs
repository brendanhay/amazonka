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
-- Module      : Network.AWS.SNS.SetEndpointAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the attributes for an endpoint for a device on one of the supported push notification services, such as GCM and APNS. For more information, see <http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications> .
--
--
module Network.AWS.SNS.SetEndpointAttributes
    (
    -- * Creating a Request
      setEndpointAttributes
    , SetEndpointAttributes
    -- * Request Lenses
    , seaEndpointARN
    , seaAttributes

    -- * Destructuring the Response
    , setEndpointAttributesResponse
    , SetEndpointAttributesResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | Input for SetEndpointAttributes action.
--
--
--
-- /See:/ 'setEndpointAttributes' smart constructor.
data SetEndpointAttributes = SetEndpointAttributes'
  { _seaEndpointARN :: !Text
  , _seaAttributes  :: !(Map Text Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetEndpointAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seaEndpointARN' - EndpointArn used for SetEndpointAttributes action.
--
-- * 'seaAttributes' - A map of the endpoint attributes. Attributes in this map include the following:     * @CustomUserData@

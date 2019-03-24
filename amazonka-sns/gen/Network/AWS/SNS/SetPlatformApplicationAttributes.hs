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
-- Module      : Network.AWS.SNS.SetPlatformApplicationAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the attributes of the platform application object for the supported push notification services, such as APNS and GCM. For more information, see <http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications> . For information on configuring attributes for message delivery status, see <http://docs.aws.amazon.com/sns/latest/dg/sns-msg-status.html Using Amazon SNS Application Attributes for Message Delivery Status> .
--
--
module Network.AWS.SNS.SetPlatformApplicationAttributes
    (
    -- * Creating a Request
      setPlatformApplicationAttributes
    , SetPlatformApplicationAttributes
    -- * Request Lenses
    , spaaPlatformApplicationARN
    , spaaAttributes

    -- * Destructuring the Response
    , setPlatformApplicationAttributesResponse
    , SetPlatformApplicationAttributesResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | Input for SetPlatformApplicationAttributes action.
--
--
--
-- /See:/ 'setPlatformApplicationAttributes' smart constructor.
data SetPlatformApplicationAttributes = SetPlatformApplicationAttributes'
  { _spaaPlatformApplicationARN :: !Text
  , _spaaAttributes             :: !(Map Text Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetPlatformApplicationAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spaaPlatformApplicationARN' - PlatformApplicationArn for SetPlatformApplicationAttributes action.
--
-- * 'spaaAttributes' - A map of the platform application attributes. Attributes in this map include the following:     * @PlatformCredential@

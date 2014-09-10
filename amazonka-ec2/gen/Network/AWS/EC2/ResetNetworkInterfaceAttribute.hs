{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ResetNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Resets a network interface attribute. You can specify only one attribute at
-- a time. Example This example resets the sourceDestCheck attribute for the
-- specified network interface.
-- https://ec2.amazonaws.com/?Action=ResetNetworkInterfaceAttribute
-- &amp;NetworkInterfaceId=eni-ffda3197 &amp;Attribute=sourceDestCheck
-- &amp;AUTHPARAMS &lt;ResetNetworkInterfaceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;5187642e-3f16-44a3-b05f-24c3848b5162&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/ResetNetworkInterfaceAttributeResponse&gt;.
module Network.AWS.EC2.ResetNetworkInterfaceAttribute
    (
    -- * Request
      ResetNetworkInterfaceAttribute
    -- ** Request constructor
    , mkResetNetworkInterfaceAttribute
    -- ** Request lenses
    , rniaNetworkInterfaceId
    , rniaSourceDestCheck

    -- * Response
    , ResetNetworkInterfaceAttributeResponse
    -- ** Response constructor
    , mkResetNetworkInterfaceAttributeResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data ResetNetworkInterfaceAttribute = ResetNetworkInterfaceAttribute
    { _rniaNetworkInterfaceId :: !Text
    , _rniaSourceDestCheck :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ResetNetworkInterfaceAttribute' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @NetworkInterfaceId ::@ @Text@
--
-- * @SourceDestCheck ::@ @Maybe Text@
--
mkResetNetworkInterfaceAttribute :: Text -- ^ 'rniaNetworkInterfaceId'
                                 -> ResetNetworkInterfaceAttribute
mkResetNetworkInterfaceAttribute p1 = ResetNetworkInterfaceAttribute
    { _rniaNetworkInterfaceId = p1
    , _rniaSourceDestCheck = Nothing
    }

-- | The ID of the network interface.
rniaNetworkInterfaceId :: Lens' ResetNetworkInterfaceAttribute Text
rniaNetworkInterfaceId =
    lens _rniaNetworkInterfaceId (\s a -> s { _rniaNetworkInterfaceId = a })

-- | Indicates whether source/destination checking is enabled. A value of true
-- means checking is enabled, and false means checking is disabled. This value
-- must be false for a NAT instance to perform NAT.
rniaSourceDestCheck :: Lens' ResetNetworkInterfaceAttribute (Maybe Text)
rniaSourceDestCheck =
    lens _rniaSourceDestCheck (\s a -> s { _rniaSourceDestCheck = a })

instance ToQuery ResetNetworkInterfaceAttribute where
    toQuery = genericQuery def

data ResetNetworkInterfaceAttributeResponse = ResetNetworkInterfaceAttributeResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ResetNetworkInterfaceAttributeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkResetNetworkInterfaceAttributeResponse :: ResetNetworkInterfaceAttributeResponse
mkResetNetworkInterfaceAttributeResponse = ResetNetworkInterfaceAttributeResponse

instance AWSRequest ResetNetworkInterfaceAttribute where
    type Sv ResetNetworkInterfaceAttribute = EC2
    type Rs ResetNetworkInterfaceAttribute = ResetNetworkInterfaceAttributeResponse

    request = post "ResetNetworkInterfaceAttribute"
    response _ = nullaryResponse ResetNetworkInterfaceAttributeResponse

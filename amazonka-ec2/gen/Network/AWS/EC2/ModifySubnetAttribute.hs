{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.ModifySubnetAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Modifies a subnet attribute.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifySubnetAttribute.html>
module Network.AWS.EC2.ModifySubnetAttribute
    (
    -- * Request
      ModifySubnetAttribute
    -- ** Request constructor
    , modifySubnetAttribute
    -- ** Request lenses
    , msaMapPublicIPOnLaunch
    , msaSubnetId

    -- * Response
    , ModifySubnetAttributeResponse
    -- ** Response constructor
    , modifySubnetAttributeResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifySubnetAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'msaMapPublicIPOnLaunch'
--
-- * 'msaSubnetId'
data ModifySubnetAttribute = ModifySubnetAttribute'{_msaMapPublicIPOnLaunch :: Maybe AttributeBooleanValue, _msaSubnetId :: Text} deriving (Eq, Read, Show)

-- | 'ModifySubnetAttribute' smart constructor.
modifySubnetAttribute :: Text -> ModifySubnetAttribute
modifySubnetAttribute pSubnetId = ModifySubnetAttribute'{_msaMapPublicIPOnLaunch = Nothing, _msaSubnetId = pSubnetId};

-- | Specify @true@ to indicate that instances launched into the specified
-- subnet should be assigned public IP address.
msaMapPublicIPOnLaunch :: Lens' ModifySubnetAttribute (Maybe AttributeBooleanValue)
msaMapPublicIPOnLaunch = lens _msaMapPublicIPOnLaunch (\ s a -> s{_msaMapPublicIPOnLaunch = a});

-- | The ID of the subnet.
msaSubnetId :: Lens' ModifySubnetAttribute Text
msaSubnetId = lens _msaSubnetId (\ s a -> s{_msaSubnetId = a});

instance AWSRequest ModifySubnetAttribute where
        type Sv ModifySubnetAttribute = EC2
        type Rs ModifySubnetAttribute =
             ModifySubnetAttributeResponse
        request = post
        response = receiveNull ModifySubnetAttributeResponse'

instance ToHeaders ModifySubnetAttribute where
        toHeaders = const mempty

instance ToPath ModifySubnetAttribute where
        toPath = const "/"

instance ToQuery ModifySubnetAttribute where
        toQuery ModifySubnetAttribute'{..}
          = mconcat
              ["Action" =: ("ModifySubnetAttribute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "MapPublicIpOnLaunch" =: _msaMapPublicIPOnLaunch,
               "SubnetId" =: _msaSubnetId]

-- | /See:/ 'modifySubnetAttributeResponse' smart constructor.
data ModifySubnetAttributeResponse = ModifySubnetAttributeResponse' deriving (Eq, Read, Show)

-- | 'ModifySubnetAttributeResponse' smart constructor.
modifySubnetAttributeResponse :: ModifySubnetAttributeResponse
modifySubnetAttributeResponse = ModifySubnetAttributeResponse';

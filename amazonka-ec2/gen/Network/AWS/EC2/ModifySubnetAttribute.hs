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
-- Module      : Network.AWS.EC2.ModifySubnetAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a subnet attribute.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifySubnetAttribute.html AWS API Reference> for ModifySubnetAttribute.
module Network.AWS.EC2.ModifySubnetAttribute
    (
    -- * Creating a Request
      modifySubnetAttribute
    , ModifySubnetAttribute
    -- * Request Lenses
    , msaMapPublicIPOnLaunch
    , msaSubnetId

    -- * Destructuring the Response
    , modifySubnetAttributeResponse
    , ModifySubnetAttributeResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'modifySubnetAttribute' smart constructor.
data ModifySubnetAttribute = ModifySubnetAttribute'
    { _msaMapPublicIPOnLaunch :: !(Maybe AttributeBooleanValue)
    , _msaSubnetId            :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifySubnetAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msaMapPublicIPOnLaunch'
--
-- * 'msaSubnetId'
modifySubnetAttribute
    :: Text -- ^ 'msaSubnetId'
    -> ModifySubnetAttribute
modifySubnetAttribute pSubnetId_ =
    ModifySubnetAttribute'
    { _msaMapPublicIPOnLaunch = Nothing
    , _msaSubnetId = pSubnetId_
    }

-- | Specify 'true' to indicate that instances launched into the specified
-- subnet should be assigned public IP address.
msaMapPublicIPOnLaunch :: Lens' ModifySubnetAttribute (Maybe AttributeBooleanValue)
msaMapPublicIPOnLaunch = lens _msaMapPublicIPOnLaunch (\ s a -> s{_msaMapPublicIPOnLaunch = a});

-- | The ID of the subnet.
msaSubnetId :: Lens' ModifySubnetAttribute Text
msaSubnetId = lens _msaSubnetId (\ s a -> s{_msaSubnetId = a});

instance AWSRequest ModifySubnetAttribute where
        type Rs ModifySubnetAttribute =
             ModifySubnetAttributeResponse
        request = postQuery eC2
        response = receiveNull ModifySubnetAttributeResponse'

instance ToHeaders ModifySubnetAttribute where
        toHeaders = const mempty

instance ToPath ModifySubnetAttribute where
        toPath = const "/"

instance ToQuery ModifySubnetAttribute where
        toQuery ModifySubnetAttribute'{..}
          = mconcat
              ["Action" =: ("ModifySubnetAttribute" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               "MapPublicIpOnLaunch" =: _msaMapPublicIPOnLaunch,
               "SubnetId" =: _msaSubnetId]

-- | /See:/ 'modifySubnetAttributeResponse' smart constructor.
data ModifySubnetAttributeResponse =
    ModifySubnetAttributeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifySubnetAttributeResponse' with the minimum fields required to make a request.
--
modifySubnetAttributeResponse
    :: ModifySubnetAttributeResponse
modifySubnetAttributeResponse = ModifySubnetAttributeResponse'

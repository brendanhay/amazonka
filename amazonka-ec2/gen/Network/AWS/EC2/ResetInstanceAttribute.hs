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
-- Module      : Network.AWS.EC2.ResetInstanceAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets an attribute of an instance to its default value. To reset the @kernel@ or @ramdisk@ , the instance must be in a stopped state. To reset the @sourceDestCheck@ , the instance can be either running or stopped.
--
--
-- The @sourceDestCheck@ attribute controls whether source/destination checking is enabled. The default value is @true@ , which means checking is enabled. This value must be @false@ for a NAT instance to perform NAT. For more information, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
--
module Network.AWS.EC2.ResetInstanceAttribute
    (
    -- * Creating a Request
      resetInstanceAttribute
    , ResetInstanceAttribute
    -- * Request Lenses
    , riaDryRun
    , riaAttribute
    , riaInstanceId

    -- * Destructuring the Response
    , resetInstanceAttributeResponse
    , ResetInstanceAttributeResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ResetInstanceAttribute.
--
--
--
-- /See:/ 'resetInstanceAttribute' smart constructor.
data ResetInstanceAttribute = ResetInstanceAttribute'
  { _riaDryRun     :: !(Maybe Bool)
  , _riaAttribute  :: !InstanceAttributeName
  , _riaInstanceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResetInstanceAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'riaAttribute' - The attribute to reset. /Important:/ You can only reset the following attributes: @kernel@ | @ramdisk@ | @sourceDestCheck@ . To change an instance attribute, use 'ModifyInstanceAttribute' .
--
-- * 'riaInstanceId' - The ID of the instance.
resetInstanceAttribute
    :: InstanceAttributeName -- ^ 'riaAttribute'
    -> Text -- ^ 'riaInstanceId'
    -> ResetInstanceAttribute
resetInstanceAttribute pAttribute_ pInstanceId_ =
  ResetInstanceAttribute'
    { _riaDryRun = Nothing
    , _riaAttribute = pAttribute_
    , _riaInstanceId = pInstanceId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
riaDryRun :: Lens' ResetInstanceAttribute (Maybe Bool)
riaDryRun = lens _riaDryRun (\ s a -> s{_riaDryRun = a})

-- | The attribute to reset. /Important:/ You can only reset the following attributes: @kernel@ | @ramdisk@ | @sourceDestCheck@ . To change an instance attribute, use 'ModifyInstanceAttribute' .
riaAttribute :: Lens' ResetInstanceAttribute InstanceAttributeName
riaAttribute = lens _riaAttribute (\ s a -> s{_riaAttribute = a})

-- | The ID of the instance.
riaInstanceId :: Lens' ResetInstanceAttribute Text
riaInstanceId = lens _riaInstanceId (\ s a -> s{_riaInstanceId = a})

instance AWSRequest ResetInstanceAttribute where
        type Rs ResetInstanceAttribute =
             ResetInstanceAttributeResponse
        request = postQuery ec2
        response
          = receiveNull ResetInstanceAttributeResponse'

instance Hashable ResetInstanceAttribute where

instance NFData ResetInstanceAttribute where

instance ToHeaders ResetInstanceAttribute where
        toHeaders = const mempty

instance ToPath ResetInstanceAttribute where
        toPath = const "/"

instance ToQuery ResetInstanceAttribute where
        toQuery ResetInstanceAttribute'{..}
          = mconcat
              ["Action" =:
                 ("ResetInstanceAttribute" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _riaDryRun, "Attribute" =: _riaAttribute,
               "InstanceId" =: _riaInstanceId]

-- | /See:/ 'resetInstanceAttributeResponse' smart constructor.
data ResetInstanceAttributeResponse =
  ResetInstanceAttributeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResetInstanceAttributeResponse' with the minimum fields required to make a request.
--
resetInstanceAttributeResponse
    :: ResetInstanceAttributeResponse
resetInstanceAttributeResponse = ResetInstanceAttributeResponse'


instance NFData ResetInstanceAttributeResponse where

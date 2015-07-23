{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ResetInstanceAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Resets an attribute of an instance to its default value. To reset the
-- @kernel@ or @ramdisk@, the instance must be in a stopped state. To reset
-- the @SourceDestCheck@, the instance can be either running or stopped.
--
-- The @SourceDestCheck@ attribute controls whether source\/destination
-- checking is enabled. The default value is @true@, which means checking
-- is enabled. This value must be @false@ for a NAT instance to perform
-- NAT. For more information, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ResetInstanceAttribute.html>
module Network.AWS.EC2.ResetInstanceAttribute
    (
    -- * Request
      ResetInstanceAttribute
    -- ** Request constructor
    , resetInstanceAttribute
    -- ** Request lenses
    , riarqDryRun
    , riarqInstanceId
    , riarqAttribute

    -- * Response
    , ResetInstanceAttributeResponse
    -- ** Response constructor
    , resetInstanceAttributeResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'resetInstanceAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'riarqDryRun'
--
-- * 'riarqInstanceId'
--
-- * 'riarqAttribute'
data ResetInstanceAttribute = ResetInstanceAttribute'
    { _riarqDryRun     :: !(Maybe Bool)
    , _riarqInstanceId :: !Text
    , _riarqAttribute  :: !InstanceAttributeName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ResetInstanceAttribute' smart constructor.
resetInstanceAttribute :: Text -> InstanceAttributeName -> ResetInstanceAttribute
resetInstanceAttribute pInstanceId_ pAttribute_ =
    ResetInstanceAttribute'
    { _riarqDryRun = Nothing
    , _riarqInstanceId = pInstanceId_
    , _riarqAttribute = pAttribute_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
riarqDryRun :: Lens' ResetInstanceAttribute (Maybe Bool)
riarqDryRun = lens _riarqDryRun (\ s a -> s{_riarqDryRun = a});

-- | The ID of the instance.
riarqInstanceId :: Lens' ResetInstanceAttribute Text
riarqInstanceId = lens _riarqInstanceId (\ s a -> s{_riarqInstanceId = a});

-- | The attribute to reset.
riarqAttribute :: Lens' ResetInstanceAttribute InstanceAttributeName
riarqAttribute = lens _riarqAttribute (\ s a -> s{_riarqAttribute = a});

instance AWSRequest ResetInstanceAttribute where
        type Sv ResetInstanceAttribute = EC2
        type Rs ResetInstanceAttribute =
             ResetInstanceAttributeResponse
        request = post
        response
          = receiveNull ResetInstanceAttributeResponse'

instance ToHeaders ResetInstanceAttribute where
        toHeaders = const mempty

instance ToPath ResetInstanceAttribute where
        toPath = const "/"

instance ToQuery ResetInstanceAttribute where
        toQuery ResetInstanceAttribute'{..}
          = mconcat
              ["Action" =:
                 ("ResetInstanceAttribute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _riarqDryRun,
               "InstanceId" =: _riarqInstanceId,
               "Attribute" =: _riarqAttribute]

-- | /See:/ 'resetInstanceAttributeResponse' smart constructor.
data ResetInstanceAttributeResponse =
    ResetInstanceAttributeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ResetInstanceAttributeResponse' smart constructor.
resetInstanceAttributeResponse :: ResetInstanceAttributeResponse
resetInstanceAttributeResponse = ResetInstanceAttributeResponse'

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
-- Module      : Network.AWS.EC2.ModifyIdFormat
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the ID format for the specified resource on a per-region basis.
-- You can specify that resources should receive longer IDs (17-character
-- IDs) when they are created. The following resource types support longer
-- IDs: 'instance' | 'reservation'.
--
-- This setting applies to the IAM user who makes the request; it does not
-- apply to the entire AWS account. By default, an IAM user defaults to the
-- same settings as the root user. If you\'re using this action as the root
-- user or as an IAM role that has permission to use this action, then
-- these settings apply to the entire account, unless an IAM user
-- explicitly overrides these settings for themselves. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/resource-ids.html#resource-ids-access Controlling Access to Longer ID Settings>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Resources created with longer IDs are visible to all IAM users,
-- regardless of these settings and provided that they have permission to
-- use the relevant 'Describe' command for the resource type.
module Network.AWS.EC2.ModifyIdFormat
    (
    -- * Creating a Request
      modifyIdFormat
    , ModifyIdFormat
    -- * Request Lenses
    , mifResource
    , mifUseLongIds

    -- * Destructuring the Response
    , modifyIdFormatResponse
    , ModifyIdFormatResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'modifyIdFormat' smart constructor.
data ModifyIdFormat = ModifyIdFormat'
    { _mifResource   :: !Text
    , _mifUseLongIds :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyIdFormat' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mifResource'
--
-- * 'mifUseLongIds'
modifyIdFormat
    :: Text -- ^ 'mifResource'
    -> Bool -- ^ 'mifUseLongIds'
    -> ModifyIdFormat
modifyIdFormat pResource_ pUseLongIds_ =
    ModifyIdFormat'
    { _mifResource = pResource_
    , _mifUseLongIds = pUseLongIds_
    }

-- | The type of resource.
mifResource :: Lens' ModifyIdFormat Text
mifResource = lens _mifResource (\ s a -> s{_mifResource = a});

-- | Indicate whether the resource should use longer IDs (17-character IDs).
mifUseLongIds :: Lens' ModifyIdFormat Bool
mifUseLongIds = lens _mifUseLongIds (\ s a -> s{_mifUseLongIds = a});

instance AWSRequest ModifyIdFormat where
        type Rs ModifyIdFormat = ModifyIdFormatResponse
        request = postQuery ec2
        response = receiveNull ModifyIdFormatResponse'

instance Hashable ModifyIdFormat

instance ToHeaders ModifyIdFormat where
        toHeaders = const mempty

instance ToPath ModifyIdFormat where
        toPath = const "/"

instance ToQuery ModifyIdFormat where
        toQuery ModifyIdFormat'{..}
          = mconcat
              ["Action" =: ("ModifyIdFormat" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               "Resource" =: _mifResource,
               "UseLongIds" =: _mifUseLongIds]

-- | /See:/ 'modifyIdFormatResponse' smart constructor.
data ModifyIdFormatResponse =
    ModifyIdFormatResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyIdFormatResponse' with the minimum fields required to make a request.
--
modifyIdFormatResponse
    :: ModifyIdFormatResponse
modifyIdFormatResponse = ModifyIdFormatResponse'

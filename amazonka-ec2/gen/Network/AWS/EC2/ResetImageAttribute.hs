{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.EC2.ResetImageAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Resets an attribute of an AMI to its default value.
--
-- The productCodes attribute can\'t be reset.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ResetImageAttribute.html>
module Network.AWS.EC2.ResetImageAttribute
    (
    -- * Request
      ResetImageAttribute
    -- ** Request constructor
    , resetImageAttribute
    -- ** Request lenses
    , resDryRun
    , resImageId
    , resAttribute

    -- * Response
    , ResetImageAttributeResponse
    -- ** Response constructor
    , resetImageAttributeResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'resetImageAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'resDryRun'
--
-- * 'resImageId'
--
-- * 'resAttribute'
data ResetImageAttribute = ResetImageAttribute'
    { _resDryRun    :: !(Maybe Bool)
    , _resImageId   :: !Text
    , _resAttribute :: !ResetImageAttributeName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ResetImageAttribute' smart constructor.
resetImageAttribute :: Text -> ResetImageAttributeName -> ResetImageAttribute
resetImageAttribute pImageId pAttribute =
    ResetImageAttribute'
    { _resDryRun = Nothing
    , _resImageId = pImageId
    , _resAttribute = pAttribute
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
resDryRun :: Lens' ResetImageAttribute (Maybe Bool)
resDryRun = lens _resDryRun (\ s a -> s{_resDryRun = a});

-- | The ID of the AMI.
resImageId :: Lens' ResetImageAttribute Text
resImageId = lens _resImageId (\ s a -> s{_resImageId = a});

-- | The attribute to reset (currently you can only reset the launch
-- permission attribute).
resAttribute :: Lens' ResetImageAttribute ResetImageAttributeName
resAttribute = lens _resAttribute (\ s a -> s{_resAttribute = a});

instance AWSRequest ResetImageAttribute where
        type Sv ResetImageAttribute = EC2
        type Rs ResetImageAttribute =
             ResetImageAttributeResponse
        request = post
        response = receiveNull ResetImageAttributeResponse'

instance ToHeaders ResetImageAttribute where
        toHeaders = const mempty

instance ToPath ResetImageAttribute where
        toPath = const "/"

instance ToQuery ResetImageAttribute where
        toQuery ResetImageAttribute'{..}
          = mconcat
              ["Action" =: ("ResetImageAttribute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _resDryRun, "ImageId" =: _resImageId,
               "Attribute" =: _resAttribute]

-- | /See:/ 'resetImageAttributeResponse' smart constructor.
data ResetImageAttributeResponse =
    ResetImageAttributeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ResetImageAttributeResponse' smart constructor.
resetImageAttributeResponse :: ResetImageAttributeResponse
resetImageAttributeResponse = ResetImageAttributeResponse'

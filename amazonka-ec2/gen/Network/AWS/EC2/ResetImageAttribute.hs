{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ResetImageAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Resets an attribute of an AMI to its default value.
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
    , resrqDryRun
    , resrqImageId
    , resrqAttribute

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
-- * 'resrqDryRun'
--
-- * 'resrqImageId'
--
-- * 'resrqAttribute'
data ResetImageAttribute = ResetImageAttribute'
    { _resrqDryRun    :: !(Maybe Bool)
    , _resrqImageId   :: !Text
    , _resrqAttribute :: !ResetImageAttributeName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ResetImageAttribute' smart constructor.
resetImageAttribute :: Text -> ResetImageAttributeName -> ResetImageAttribute
resetImageAttribute pImageId_ pAttribute_ =
    ResetImageAttribute'
    { _resrqDryRun = Nothing
    , _resrqImageId = pImageId_
    , _resrqAttribute = pAttribute_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
resrqDryRun :: Lens' ResetImageAttribute (Maybe Bool)
resrqDryRun = lens _resrqDryRun (\ s a -> s{_resrqDryRun = a});

-- | The ID of the AMI.
resrqImageId :: Lens' ResetImageAttribute Text
resrqImageId = lens _resrqImageId (\ s a -> s{_resrqImageId = a});

-- | The attribute to reset (currently you can only reset the launch
-- permission attribute).
resrqAttribute :: Lens' ResetImageAttribute ResetImageAttributeName
resrqAttribute = lens _resrqAttribute (\ s a -> s{_resrqAttribute = a});

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
               "DryRun" =: _resrqDryRun, "ImageId" =: _resrqImageId,
               "Attribute" =: _resrqAttribute]

-- | /See:/ 'resetImageAttributeResponse' smart constructor.
data ResetImageAttributeResponse =
    ResetImageAttributeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ResetImageAttributeResponse' smart constructor.
resetImageAttributeResponse :: ResetImageAttributeResponse
resetImageAttributeResponse = ResetImageAttributeResponse'

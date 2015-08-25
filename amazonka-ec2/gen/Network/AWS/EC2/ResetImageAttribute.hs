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
-- Module      : Network.AWS.EC2.ResetImageAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets an attribute of an AMI to its default value.
--
-- The productCodes attribute can\'t be reset.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ResetImageAttribute.html AWS API Reference> for ResetImageAttribute.
module Network.AWS.EC2.ResetImageAttribute
    (
    -- * Creating a Request
      resetImageAttribute
    , ResetImageAttribute
    -- * Request Lenses
    , resDryRun
    , resImageId
    , resAttribute

    -- * Destructuring the Response
    , resetImageAttributeResponse
    , ResetImageAttributeResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'resetImageAttribute' smart constructor.
data ResetImageAttribute = ResetImageAttribute'
    { _resDryRun    :: !(Maybe Bool)
    , _resImageId   :: !Text
    , _resAttribute :: !ResetImageAttributeName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResetImageAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'resDryRun'
--
-- * 'resImageId'
--
-- * 'resAttribute'
resetImageAttribute
    :: Text -- ^ 'resImageId'
    -> ResetImageAttributeName -- ^ 'resAttribute'
    -> ResetImageAttribute
resetImageAttribute pImageId_ pAttribute_ =
    ResetImageAttribute'
    { _resDryRun = Nothing
    , _resImageId = pImageId_
    , _resAttribute = pAttribute_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
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
        type Rs ResetImageAttribute =
             ResetImageAttributeResponse
        request = postQuery eC2
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

-- | Creates a value of 'ResetImageAttributeResponse' with the minimum fields required to make a request.
--
resetImageAttributeResponse
    :: ResetImageAttributeResponse
resetImageAttributeResponse = ResetImageAttributeResponse'

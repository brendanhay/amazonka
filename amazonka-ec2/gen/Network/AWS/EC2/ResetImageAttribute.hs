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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets an attribute of an AMI to its default value.
--
--
module Network.AWS.EC2.ResetImageAttribute
    (
    -- * Creating a Request
      resetImageAttribute
    , ResetImageAttribute
    -- * Request Lenses
    , resDryRun
    , resAttribute
    , resImageId

    -- * Destructuring the Response
    , resetImageAttributeResponse
    , ResetImageAttributeResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ResetImageAttribute.
--
--
--
-- /See:/ 'resetImageAttribute' smart constructor.
data ResetImageAttribute = ResetImageAttribute'
  { _resDryRun    :: !(Maybe Bool)
  , _resAttribute :: !ResetImageAttributeName
  , _resImageId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResetImageAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'resDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'resAttribute' - The attribute to reset (currently you can only reset the launch permission attribute).
--
-- * 'resImageId' - The ID of the AMI.
resetImageAttribute
    :: ResetImageAttributeName -- ^ 'resAttribute'
    -> Text -- ^ 'resImageId'
    -> ResetImageAttribute
resetImageAttribute pAttribute_ pImageId_ =
  ResetImageAttribute'
    {_resDryRun = Nothing, _resAttribute = pAttribute_, _resImageId = pImageId_}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
resDryRun :: Lens' ResetImageAttribute (Maybe Bool)
resDryRun = lens _resDryRun (\ s a -> s{_resDryRun = a})

-- | The attribute to reset (currently you can only reset the launch permission attribute).
resAttribute :: Lens' ResetImageAttribute ResetImageAttributeName
resAttribute = lens _resAttribute (\ s a -> s{_resAttribute = a})

-- | The ID of the AMI.
resImageId :: Lens' ResetImageAttribute Text
resImageId = lens _resImageId (\ s a -> s{_resImageId = a})

instance AWSRequest ResetImageAttribute where
        type Rs ResetImageAttribute =
             ResetImageAttributeResponse
        request = postQuery ec2
        response = receiveNull ResetImageAttributeResponse'

instance Hashable ResetImageAttribute where

instance NFData ResetImageAttribute where

instance ToHeaders ResetImageAttribute where
        toHeaders = const mempty

instance ToPath ResetImageAttribute where
        toPath = const "/"

instance ToQuery ResetImageAttribute where
        toQuery ResetImageAttribute'{..}
          = mconcat
              ["Action" =: ("ResetImageAttribute" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _resDryRun, "Attribute" =: _resAttribute,
               "ImageId" =: _resImageId]

-- | /See:/ 'resetImageAttributeResponse' smart constructor.
data ResetImageAttributeResponse =
  ResetImageAttributeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResetImageAttributeResponse' with the minimum fields required to make a request.
--
resetImageAttributeResponse
    :: ResetImageAttributeResponse
resetImageAttributeResponse = ResetImageAttributeResponse'


instance NFData ResetImageAttributeResponse where

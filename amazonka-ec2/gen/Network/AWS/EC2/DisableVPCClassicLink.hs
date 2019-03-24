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
-- Module      : Network.AWS.EC2.DisableVPCClassicLink
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables ClassicLink for a VPC. You cannot disable ClassicLink for a VPC that has EC2-Classic instances linked to it.
--
--
module Network.AWS.EC2.DisableVPCClassicLink
    (
    -- * Creating a Request
      disableVPCClassicLink
    , DisableVPCClassicLink
    -- * Request Lenses
    , dvpcclDryRun
    , dvpcclVPCId

    -- * Destructuring the Response
    , disableVPCClassicLinkResponse
    , DisableVPCClassicLinkResponse
    -- * Response Lenses
    , dvpcclrsReturn
    , dvpcclrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disableVPCClassicLink' smart constructor.
data DisableVPCClassicLink = DisableVPCClassicLink'
  { _dvpcclDryRun :: !(Maybe Bool)
  , _dvpcclVPCId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableVPCClassicLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvpcclDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dvpcclVPCId' - The ID of the VPC.
disableVPCClassicLink
    :: Text -- ^ 'dvpcclVPCId'
    -> DisableVPCClassicLink
disableVPCClassicLink pVPCId_ =
  DisableVPCClassicLink' {_dvpcclDryRun = Nothing, _dvpcclVPCId = pVPCId_}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dvpcclDryRun :: Lens' DisableVPCClassicLink (Maybe Bool)
dvpcclDryRun = lens _dvpcclDryRun (\ s a -> s{_dvpcclDryRun = a})

-- | The ID of the VPC.
dvpcclVPCId :: Lens' DisableVPCClassicLink Text
dvpcclVPCId = lens _dvpcclVPCId (\ s a -> s{_dvpcclVPCId = a})

instance AWSRequest DisableVPCClassicLink where
        type Rs DisableVPCClassicLink =
             DisableVPCClassicLinkResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DisableVPCClassicLinkResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable DisableVPCClassicLink where

instance NFData DisableVPCClassicLink where

instance ToHeaders DisableVPCClassicLink where
        toHeaders = const mempty

instance ToPath DisableVPCClassicLink where
        toPath = const "/"

instance ToQuery DisableVPCClassicLink where
        toQuery DisableVPCClassicLink'{..}
          = mconcat
              ["Action" =: ("DisableVpcClassicLink" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dvpcclDryRun, "VpcId" =: _dvpcclVPCId]

-- | /See:/ 'disableVPCClassicLinkResponse' smart constructor.
data DisableVPCClassicLinkResponse = DisableVPCClassicLinkResponse'
  { _dvpcclrsReturn         :: !(Maybe Bool)
  , _dvpcclrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableVPCClassicLinkResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvpcclrsReturn' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- * 'dvpcclrsResponseStatus' - -- | The response status code.
disableVPCClassicLinkResponse
    :: Int -- ^ 'dvpcclrsResponseStatus'
    -> DisableVPCClassicLinkResponse
disableVPCClassicLinkResponse pResponseStatus_ =
  DisableVPCClassicLinkResponse'
    {_dvpcclrsReturn = Nothing, _dvpcclrsResponseStatus = pResponseStatus_}


-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
dvpcclrsReturn :: Lens' DisableVPCClassicLinkResponse (Maybe Bool)
dvpcclrsReturn = lens _dvpcclrsReturn (\ s a -> s{_dvpcclrsReturn = a})

-- | -- | The response status code.
dvpcclrsResponseStatus :: Lens' DisableVPCClassicLinkResponse Int
dvpcclrsResponseStatus = lens _dvpcclrsResponseStatus (\ s a -> s{_dvpcclrsResponseStatus = a})

instance NFData DisableVPCClassicLinkResponse where

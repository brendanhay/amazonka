{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DisableVPCClassicLink
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

-- | Disables ClassicLink for a VPC. You cannot disable ClassicLink for a VPC
-- that has EC2-Classic instances linked to it.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DisableVPCClassicLink.html>
module Network.AWS.EC2.DisableVPCClassicLink
    (
    -- * Request
      DisableVPCClassicLink
    -- ** Request constructor
    , disableVPCClassicLink
    -- ** Request lenses
    , disDryRun
    , disVPCId

    -- * Response
    , DisableVPCClassicLinkResponse
    -- ** Response constructor
    , disableVPCClassicLinkResponse
    -- ** Response lenses
    , disReturn
    , disStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'disableVPCClassicLink' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'disDryRun'
--
-- * 'disVPCId'
data DisableVPCClassicLink = DisableVPCClassicLink'
    { _disDryRun :: Maybe Bool
    , _disVPCId  :: Text
    } deriving (Eq,Read,Show)

-- | 'DisableVPCClassicLink' smart constructor.
disableVPCClassicLink :: Text -> DisableVPCClassicLink
disableVPCClassicLink pVPCId =
    DisableVPCClassicLink'
    { _disDryRun = Nothing
    , _disVPCId = pVPCId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
disDryRun :: Lens' DisableVPCClassicLink (Maybe Bool)
disDryRun = lens _disDryRun (\ s a -> s{_disDryRun = a});

-- | The ID of the VPC.
disVPCId :: Lens' DisableVPCClassicLink Text
disVPCId = lens _disVPCId (\ s a -> s{_disVPCId = a});

instance AWSRequest DisableVPCClassicLink where
        type Sv DisableVPCClassicLink = EC2
        type Rs DisableVPCClassicLink =
             DisableVPCClassicLinkResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DisableVPCClassicLinkResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance ToHeaders DisableVPCClassicLink where
        toHeaders = const mempty

instance ToPath DisableVPCClassicLink where
        toPath = const "/"

instance ToQuery DisableVPCClassicLink where
        toQuery DisableVPCClassicLink'{..}
          = mconcat
              ["Action" =: ("DisableVPCClassicLink" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _disDryRun, "VpcId" =: _disVPCId]

-- | /See:/ 'disableVPCClassicLinkResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'disReturn'
--
-- * 'disStatus'
data DisableVPCClassicLinkResponse = DisableVPCClassicLinkResponse'
    { _disReturn :: Maybe Bool
    , _disStatus :: !Int
    } deriving (Eq,Read,Show)

-- | 'DisableVPCClassicLinkResponse' smart constructor.
disableVPCClassicLinkResponse :: Int -> DisableVPCClassicLinkResponse
disableVPCClassicLinkResponse pStatus =
    DisableVPCClassicLinkResponse'
    { _disReturn = Nothing
    , _disStatus = pStatus
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
disReturn :: Lens' DisableVPCClassicLinkResponse (Maybe Bool)
disReturn = lens _disReturn (\ s a -> s{_disReturn = a});

-- | FIXME: Undocumented member.
disStatus :: Lens' DisableVPCClassicLinkResponse Int
disStatus = lens _disStatus (\ s a -> s{_disStatus = a});

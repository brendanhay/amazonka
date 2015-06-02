{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CancelSpotFleetRequests
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Cancels the specified Spot fleet requests.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelSpotFleetRequests.html>
module Network.AWS.EC2.CancelSpotFleetRequests
    (
    -- * Request
      CancelSpotFleetRequests
    -- ** Request constructor
    , cancelSpotFleetRequests
    -- ** Request lenses
    , csfrDryRun
    , csfrSpotFleetRequestIds
    , csfrTerminateInstances

    -- * Response
    , CancelSpotFleetRequestsResponse
    -- ** Response constructor
    , cancelSpotFleetRequestsResponse
    -- ** Response lenses
    , csfrrSuccessfulFleetRequests
    , csfrrUnsuccessfulFleetRequests
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CancelSpotFleetRequests = CancelSpotFleetRequests
    { _csfrDryRun              :: Maybe Bool
    , _csfrSpotFleetRequestIds :: List "item" Text
    , _csfrTerminateInstances  :: Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'CancelSpotFleetRequests' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csfrDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'csfrSpotFleetRequestIds' @::@ ['Text']
--
-- * 'csfrTerminateInstances' @::@ 'Bool'
--
cancelSpotFleetRequests :: Bool -- ^ 'csfrTerminateInstances'
                        -> CancelSpotFleetRequests
cancelSpotFleetRequests p1 = CancelSpotFleetRequests
    { _csfrTerminateInstances  = p1
    , _csfrDryRun              = Nothing
    , _csfrSpotFleetRequestIds = mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
csfrDryRun :: Lens' CancelSpotFleetRequests (Maybe Bool)
csfrDryRun = lens _csfrDryRun (\s a -> s { _csfrDryRun = a })

-- | The IDs of the Spot fleet requests.
csfrSpotFleetRequestIds :: Lens' CancelSpotFleetRequests [Text]
csfrSpotFleetRequestIds =
    lens _csfrSpotFleetRequestIds (\s a -> s { _csfrSpotFleetRequestIds = a })
        . _List

-- | Indicates whether to terminate instances for a Spot fleet request if it is
-- canceled successfully.
csfrTerminateInstances :: Lens' CancelSpotFleetRequests Bool
csfrTerminateInstances =
    lens _csfrTerminateInstances (\s a -> s { _csfrTerminateInstances = a })

data CancelSpotFleetRequestsResponse = CancelSpotFleetRequestsResponse
    { _csfrrSuccessfulFleetRequests   :: List "item" CancelSpotFleetRequestsSuccessItem
    , _csfrrUnsuccessfulFleetRequests :: List "item" CancelSpotFleetRequestsErrorItem
    } deriving (Eq, Read, Show)

-- | 'CancelSpotFleetRequestsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csfrrSuccessfulFleetRequests' @::@ ['CancelSpotFleetRequestsSuccessItem']
--
-- * 'csfrrUnsuccessfulFleetRequests' @::@ ['CancelSpotFleetRequestsErrorItem']
--
cancelSpotFleetRequestsResponse :: CancelSpotFleetRequestsResponse
cancelSpotFleetRequestsResponse = CancelSpotFleetRequestsResponse
    { _csfrrUnsuccessfulFleetRequests = mempty
    , _csfrrSuccessfulFleetRequests   = mempty
    }

-- | Information about the Spot fleet requests that are successfully canceled.
csfrrSuccessfulFleetRequests :: Lens' CancelSpotFleetRequestsResponse [CancelSpotFleetRequestsSuccessItem]
csfrrSuccessfulFleetRequests =
    lens _csfrrSuccessfulFleetRequests
        (\s a -> s { _csfrrSuccessfulFleetRequests = a })
            . _List

-- | Information about the Spot fleet requests that are not successfully canceled.
csfrrUnsuccessfulFleetRequests :: Lens' CancelSpotFleetRequestsResponse [CancelSpotFleetRequestsErrorItem]
csfrrUnsuccessfulFleetRequests =
    lens _csfrrUnsuccessfulFleetRequests
        (\s a -> s { _csfrrUnsuccessfulFleetRequests = a })
            . _List

instance ToPath CancelSpotFleetRequests where
    toPath = const "/"

instance ToQuery CancelSpotFleetRequests where
    toQuery CancelSpotFleetRequests{..} = mconcat
        [ "DryRun"             =? _csfrDryRun
        , "SpotFleetRequestId" `toQueryList` _csfrSpotFleetRequestIds
        , "TerminateInstances" =? _csfrTerminateInstances
        ]

instance ToHeaders CancelSpotFleetRequests

instance AWSRequest CancelSpotFleetRequests where
    type Sv CancelSpotFleetRequests = EC2
    type Rs CancelSpotFleetRequests = CancelSpotFleetRequestsResponse

    request  = post "CancelSpotFleetRequests"
    response = xmlResponse

instance FromXML CancelSpotFleetRequestsResponse where
    parseXML x = CancelSpotFleetRequestsResponse
        <$> x .@? "successfulFleetRequestSet" .!@ mempty
        <*> x .@? "unsuccessfulFleetRequestSet" .!@ mempty

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

-- Module      : Network.AWS.EC2.RestoreAddressToClassic
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

-- | Restores an Elastic IP address that was previously moved to the EC2-VPC
-- platform back to the EC2-Classic platform. You cannot move an Elastic IP
-- address that was originally allocated for use in EC2-VPC. The Elastic IP
-- address must not be associated with an instance or network interface.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RestoreAddressToClassic.html>
module Network.AWS.EC2.RestoreAddressToClassic
    (
    -- * Request
      RestoreAddressToClassic
    -- ** Request constructor
    , restoreAddressToClassic
    -- ** Request lenses
    , ratcDryRun
    , ratcPublicIp

    -- * Response
    , RestoreAddressToClassicResponse
    -- ** Response constructor
    , restoreAddressToClassicResponse
    -- ** Response lenses
    , ratcrPublicIp
    , ratcrStatus
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data RestoreAddressToClassic = RestoreAddressToClassic
    { _ratcDryRun   :: Maybe Bool
    , _ratcPublicIp :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'RestoreAddressToClassic' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ratcDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'ratcPublicIp' @::@ 'Text'
--
restoreAddressToClassic :: Text -- ^ 'ratcPublicIp'
                        -> RestoreAddressToClassic
restoreAddressToClassic p1 = RestoreAddressToClassic
    { _ratcPublicIp = p1
    , _ratcDryRun   = Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
ratcDryRun :: Lens' RestoreAddressToClassic (Maybe Bool)
ratcDryRun = lens _ratcDryRun (\s a -> s { _ratcDryRun = a })

-- | The Elastic IP address.
ratcPublicIp :: Lens' RestoreAddressToClassic Text
ratcPublicIp = lens _ratcPublicIp (\s a -> s { _ratcPublicIp = a })

data RestoreAddressToClassicResponse = RestoreAddressToClassicResponse
    { _ratcrPublicIp :: Maybe Text
    , _ratcrStatus   :: Maybe Status
    } deriving (Eq, Read, Show)

-- | 'RestoreAddressToClassicResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ratcrPublicIp' @::@ 'Maybe' 'Text'
--
-- * 'ratcrStatus' @::@ 'Maybe' 'Status'
--
restoreAddressToClassicResponse :: RestoreAddressToClassicResponse
restoreAddressToClassicResponse = RestoreAddressToClassicResponse
    { _ratcrStatus   = Nothing
    , _ratcrPublicIp = Nothing
    }

-- | The Elastic IP address.
ratcrPublicIp :: Lens' RestoreAddressToClassicResponse (Maybe Text)
ratcrPublicIp = lens _ratcrPublicIp (\s a -> s { _ratcrPublicIp = a })

-- | The move status for the IP address.
ratcrStatus :: Lens' RestoreAddressToClassicResponse (Maybe Status)
ratcrStatus = lens _ratcrStatus (\s a -> s { _ratcrStatus = a })

instance ToPath RestoreAddressToClassic where
    toPath = const "/"

instance ToQuery RestoreAddressToClassic where
    toQuery RestoreAddressToClassic{..} = mconcat
        [ "DryRun"   =? _ratcDryRun
        , "PublicIp" =? _ratcPublicIp
        ]

instance ToHeaders RestoreAddressToClassic

instance AWSRequest RestoreAddressToClassic where
    type Sv RestoreAddressToClassic = EC2
    type Rs RestoreAddressToClassic = RestoreAddressToClassicResponse

    request  = post "RestoreAddressToClassic"
    response = xmlResponse

instance FromXML RestoreAddressToClassicResponse where
    parseXML x = RestoreAddressToClassicResponse
        <$> x .@? "publicIp"
        <*> x .@? "status"

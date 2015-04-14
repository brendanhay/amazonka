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

-- Module      : Network.AWS.StorageGateway.ListLocalDisks
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

-- | This operation returns a list of the gateway's local disks. To specify which
-- gateway to describe, you use the Amazon Resource Name (ARN) of the gateway in
-- the body of the request.
--
-- The request returns a list of all disks, specifying which are configured as
-- working storage, cache storage, or stored volume or not configured at all.
-- The response includes a 'DiskStatus' field. This field can have a value of
-- present (the disk is availble to use), missing (the disk is no longer
-- connected to the gateway), or mismatch (the disk node is occupied by a disk
-- that has incorrect metadata or the disk content is corrupted).
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_ListLocalDisks.html>
module Network.AWS.StorageGateway.ListLocalDisks
    (
    -- * Request
      ListLocalDisks
    -- ** Request constructor
    , listLocalDisks
    -- ** Request lenses
    , lldGatewayARN

    -- * Response
    , ListLocalDisksResponse
    -- ** Response constructor
    , listLocalDisksResponse
    -- ** Response lenses
    , lldrDisks
    , lldrGatewayARN
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

newtype ListLocalDisks = ListLocalDisks
    { _lldGatewayARN :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'ListLocalDisks' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lldGatewayARN' @::@ 'Text'
--
listLocalDisks :: Text -- ^ 'lldGatewayARN'
               -> ListLocalDisks
listLocalDisks p1 = ListLocalDisks
    { _lldGatewayARN = p1
    }

lldGatewayARN :: Lens' ListLocalDisks Text
lldGatewayARN = lens _lldGatewayARN (\s a -> s { _lldGatewayARN = a })

data ListLocalDisksResponse = ListLocalDisksResponse
    { _lldrDisks      :: List "Disks" Disk
    , _lldrGatewayARN :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ListLocalDisksResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lldrDisks' @::@ ['Disk']
--
-- * 'lldrGatewayARN' @::@ 'Maybe' 'Text'
--
listLocalDisksResponse :: ListLocalDisksResponse
listLocalDisksResponse = ListLocalDisksResponse
    { _lldrGatewayARN = Nothing
    , _lldrDisks      = mempty
    }

lldrDisks :: Lens' ListLocalDisksResponse [Disk]
lldrDisks = lens _lldrDisks (\s a -> s { _lldrDisks = a }) . _List

lldrGatewayARN :: Lens' ListLocalDisksResponse (Maybe Text)
lldrGatewayARN = lens _lldrGatewayARN (\s a -> s { _lldrGatewayARN = a })

instance ToPath ListLocalDisks where
    toPath = const "/"

instance ToQuery ListLocalDisks where
    toQuery = const mempty

instance ToHeaders ListLocalDisks

instance ToJSON ListLocalDisks where
    toJSON ListLocalDisks{..} = object
        [ "GatewayARN" .= _lldGatewayARN
        ]

instance AWSRequest ListLocalDisks where
    type Sv ListLocalDisks = StorageGateway
    type Rs ListLocalDisks = ListLocalDisksResponse

    request  = post "ListLocalDisks"
    response = jsonResponse

instance FromJSON ListLocalDisksResponse where
    parseJSON = withObject "ListLocalDisksResponse" $ \o -> ListLocalDisksResponse
        <$> o .:? "Disks" .!= mempty
        <*> o .:? "GatewayARN"

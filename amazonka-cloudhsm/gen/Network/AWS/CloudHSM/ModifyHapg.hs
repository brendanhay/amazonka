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

-- Module      : Network.AWS.CloudHSM.ModifyHapg
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

-- | Modifies an existing high-availability partition group.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_ModifyHapg.html>
module Network.AWS.CloudHSM.ModifyHapg
    (
    -- * Request
      ModifyHapg
    -- ** Request constructor
    , modifyHapg
    -- ** Request lenses
    , mhHapgArn
    , mhLabel
    , mhPartitionSerialList

    -- * Response
    , ModifyHapgResponse
    -- ** Response constructor
    , modifyHapgResponse
    -- ** Response lenses
    , mhrHapgArn
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudHSM.Types
import qualified GHC.Exts

data ModifyHapg = ModifyHapg
    { _mhHapgArn             :: Text
    , _mhLabel               :: Maybe Text
    , _mhPartitionSerialList :: List "PartitionSerialList" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ModifyHapg' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mhHapgArn' @::@ 'Text'
--
-- * 'mhLabel' @::@ 'Maybe' 'Text'
--
-- * 'mhPartitionSerialList' @::@ ['Text']
--
modifyHapg :: Text -- ^ 'mhHapgArn'
           -> ModifyHapg
modifyHapg p1 = ModifyHapg
    { _mhHapgArn             = p1
    , _mhLabel               = Nothing
    , _mhPartitionSerialList = mempty
    }

-- | The ARN of the high-availability partition group to modify.
mhHapgArn :: Lens' ModifyHapg Text
mhHapgArn = lens _mhHapgArn (\s a -> s { _mhHapgArn = a })

-- | The new label for the high-availability partition group.
mhLabel :: Lens' ModifyHapg (Maybe Text)
mhLabel = lens _mhLabel (\s a -> s { _mhLabel = a })

-- | The list of partition serial numbers to make members of the high-availability
-- partition group.
mhPartitionSerialList :: Lens' ModifyHapg [Text]
mhPartitionSerialList =
    lens _mhPartitionSerialList (\s a -> s { _mhPartitionSerialList = a })
        . _List

newtype ModifyHapgResponse = ModifyHapgResponse
    { _mhrHapgArn :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'ModifyHapgResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mhrHapgArn' @::@ 'Maybe' 'Text'
--
modifyHapgResponse :: ModifyHapgResponse
modifyHapgResponse = ModifyHapgResponse
    { _mhrHapgArn = Nothing
    }

-- | The ARN of the high-availability partition group.
mhrHapgArn :: Lens' ModifyHapgResponse (Maybe Text)
mhrHapgArn = lens _mhrHapgArn (\s a -> s { _mhrHapgArn = a })

instance ToPath ModifyHapg where
    toPath = const "/"

instance ToQuery ModifyHapg where
    toQuery = const mempty

instance ToHeaders ModifyHapg

instance ToJSON ModifyHapg where
    toJSON ModifyHapg{..} = object
        [ "HapgArn"             .= _mhHapgArn
        , "Label"               .= _mhLabel
        , "PartitionSerialList" .= _mhPartitionSerialList
        ]

instance AWSRequest ModifyHapg where
    type Sv ModifyHapg = CloudHSM
    type Rs ModifyHapg = ModifyHapgResponse

    request  = post "ModifyHapg"
    response = jsonResponse

instance FromJSON ModifyHapgResponse where
    parseJSON = withObject "ModifyHapgResponse" $ \o -> ModifyHapgResponse
        <$> o .:? "HapgArn"

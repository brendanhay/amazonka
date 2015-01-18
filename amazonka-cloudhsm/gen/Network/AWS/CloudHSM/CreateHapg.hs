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

-- Module      : Network.AWS.CloudHSM.CreateHapg
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

-- | Creates a high-availability partition group. A high-availability partition
-- group is a group of partitions that spans multiple physical HSMs.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_CreateHapg.html>
module Network.AWS.CloudHSM.CreateHapg
    (
    -- * Request
      CreateHapg
    -- ** Request constructor
    , createHapg
    -- ** Request lenses
    , chLabel

    -- * Response
    , CreateHapgResponse
    -- ** Response constructor
    , createHapgResponse
    -- ** Response lenses
    , chrHapgArn
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudHSM.Types
import qualified GHC.Exts

newtype CreateHapg = CreateHapg
    { _chLabel :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'CreateHapg' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chLabel' @::@ 'Text'
--
createHapg :: Text -- ^ 'chLabel'
           -> CreateHapg
createHapg p1 = CreateHapg
    { _chLabel = p1
    }

-- | The label of the new high-availability partition group.
chLabel :: Lens' CreateHapg Text
chLabel = lens _chLabel (\s a -> s { _chLabel = a })

newtype CreateHapgResponse = CreateHapgResponse
    { _chrHapgArn :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'CreateHapgResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chrHapgArn' @::@ 'Maybe' 'Text'
--
createHapgResponse :: CreateHapgResponse
createHapgResponse = CreateHapgResponse
    { _chrHapgArn = Nothing
    }

-- | The ARN of the high-availability partition group.
chrHapgArn :: Lens' CreateHapgResponse (Maybe Text)
chrHapgArn = lens _chrHapgArn (\s a -> s { _chrHapgArn = a })

instance ToPath CreateHapg where
    toPath = const "/"

instance ToQuery CreateHapg where
    toQuery = const mempty

instance ToHeaders CreateHapg

instance ToJSON CreateHapg where
    toJSON CreateHapg{..} = object
        [ "Label" .= _chLabel
        ]

instance AWSRequest CreateHapg where
    type Sv CreateHapg = CloudHSM
    type Rs CreateHapg = CreateHapgResponse

    request  = post "CreateHapg"
    response = jsonResponse

instance FromJSON CreateHapgResponse where
    parseJSON = withObject "CreateHapgResponse" $ \o -> CreateHapgResponse
        <$> o .:? "HapgArn"

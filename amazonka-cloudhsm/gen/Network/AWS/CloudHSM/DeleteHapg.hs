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

-- Module      : Network.AWS.CloudHSM.DeleteHapg
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

-- | Deletes a high-availability partition group.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_DeleteHapg.html>
module Network.AWS.CloudHSM.DeleteHapg
    (
    -- * Request
      DeleteHapg
    -- ** Request constructor
    , deleteHapg
    -- ** Request lenses
    , dhHapgArn

    -- * Response
    , DeleteHapgResponse
    -- ** Response constructor
    , deleteHapgResponse
    -- ** Response lenses
    , dhrStatus
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudHSM.Types
import qualified GHC.Exts

newtype DeleteHapg = DeleteHapg
    { _dhHapgArn :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DeleteHapg' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhHapgArn' @::@ 'Text'
--
deleteHapg :: Text -- ^ 'dhHapgArn'
           -> DeleteHapg
deleteHapg p1 = DeleteHapg
    { _dhHapgArn = p1
    }

-- | The ARN of the high-availability partition group to delete.
dhHapgArn :: Lens' DeleteHapg Text
dhHapgArn = lens _dhHapgArn (\s a -> s { _dhHapgArn = a })

newtype DeleteHapgResponse = DeleteHapgResponse
    { _dhrStatus :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DeleteHapgResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhrStatus' @::@ 'Text'
--
deleteHapgResponse :: Text -- ^ 'dhrStatus'
                   -> DeleteHapgResponse
deleteHapgResponse p1 = DeleteHapgResponse
    { _dhrStatus = p1
    }

-- | The status of the action.
dhrStatus :: Lens' DeleteHapgResponse Text
dhrStatus = lens _dhrStatus (\s a -> s { _dhrStatus = a })

instance ToPath DeleteHapg where
    toPath = const "/"

instance ToQuery DeleteHapg where
    toQuery = const mempty

instance ToHeaders DeleteHapg

instance ToJSON DeleteHapg where
    toJSON DeleteHapg{..} = object
        [ "HapgArn" .= _dhHapgArn
        ]

instance AWSRequest DeleteHapg where
    type Sv DeleteHapg = CloudHSM
    type Rs DeleteHapg = DeleteHapgResponse

    request  = post "DeleteHapg"
    response = jsonResponse

instance FromJSON DeleteHapgResponse where
    parseJSON = withObject "DeleteHapgResponse" $ \o -> DeleteHapgResponse
        <$> o .:  "Status"

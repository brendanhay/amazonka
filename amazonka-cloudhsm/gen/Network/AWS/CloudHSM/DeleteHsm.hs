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

-- Module      : Network.AWS.CloudHSM.DeleteHsm
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

-- | Deletes an HSM. Once complete, this operation cannot be undone and your key
-- material cannot be recovered.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_DeleteHsm.html>
module Network.AWS.CloudHSM.DeleteHsm
    (
    -- * Request
      DeleteHsm
    -- ** Request constructor
    , deleteHsm
    -- ** Request lenses
    , dhHsmArn

    -- * Response
    , DeleteHsmResponse
    -- ** Response constructor
    , deleteHsmResponse
    -- ** Response lenses
    , dhr1Status
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudHSM.Types
import qualified GHC.Exts

newtype DeleteHsm = DeleteHsm
    { _dhHsmArn :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DeleteHsm' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhHsmArn' @::@ 'Text'
--
deleteHsm :: Text -- ^ 'dhHsmArn'
          -> DeleteHsm
deleteHsm p1 = DeleteHsm
    { _dhHsmArn = p1
    }

-- | The ARN of the HSM to delete.
dhHsmArn :: Lens' DeleteHsm Text
dhHsmArn = lens _dhHsmArn (\s a -> s { _dhHsmArn = a })

newtype DeleteHsmResponse = DeleteHsmResponse
    { _dhr1Status :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DeleteHsmResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhr1Status' @::@ 'Text'
--
deleteHsmResponse :: Text -- ^ 'dhr1Status'
                  -> DeleteHsmResponse
deleteHsmResponse p1 = DeleteHsmResponse
    { _dhr1Status = p1
    }

-- | The status of the action.
dhr1Status :: Lens' DeleteHsmResponse Text
dhr1Status = lens _dhr1Status (\s a -> s { _dhr1Status = a })

instance ToPath DeleteHsm where
    toPath = const "/"

instance ToQuery DeleteHsm where
    toQuery = const mempty

instance ToHeaders DeleteHsm

instance ToJSON DeleteHsm where
    toJSON DeleteHsm{..} = object
        [ "HsmArn" .= _dhHsmArn
        ]

instance AWSRequest DeleteHsm where
    type Sv DeleteHsm = CloudHSM
    type Rs DeleteHsm = DeleteHsmResponse

    request  = post "DeleteHsm"
    response = jsonResponse

instance FromJSON DeleteHsmResponse where
    parseJSON = withObject "DeleteHsmResponse" $ \o -> DeleteHsmResponse
        <$> o .:  "Status"

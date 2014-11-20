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

-- Module      : Network.AWS.KMS.UpdateKeyDescription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | 
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_UpdateKeyDescription.html>
module Network.AWS.KMS.UpdateKeyDescription
    (
    -- * Request
      UpdateKeyDescription
    -- ** Request constructor
    , updateKeyDescription
    -- ** Request lenses
    , ukdDescription
    , ukdKeyId

    -- * Response
    , UpdateKeyDescriptionResponse
    -- ** Response constructor
    , updateKeyDescriptionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.KMS.Types
import qualified GHC.Exts

data UpdateKeyDescription = UpdateKeyDescription
    { _ukdDescription :: Text
    , _ukdKeyId       :: Text
    } deriving (Eq, Ord, Show)

-- | 'UpdateKeyDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ukdDescription' @::@ 'Text'
--
-- * 'ukdKeyId' @::@ 'Text'
--
updateKeyDescription :: Text -- ^ 'ukdKeyId'
                     -> Text -- ^ 'ukdDescription'
                     -> UpdateKeyDescription
updateKeyDescription p1 p2 = UpdateKeyDescription
    { _ukdKeyId       = p1
    , _ukdDescription = p2
    }

ukdDescription :: Lens' UpdateKeyDescription Text
ukdDescription = lens _ukdDescription (\s a -> s { _ukdDescription = a })

ukdKeyId :: Lens' UpdateKeyDescription Text
ukdKeyId = lens _ukdKeyId (\s a -> s { _ukdKeyId = a })

data UpdateKeyDescriptionResponse = UpdateKeyDescriptionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UpdateKeyDescriptionResponse' constructor.
updateKeyDescriptionResponse :: UpdateKeyDescriptionResponse
updateKeyDescriptionResponse = UpdateKeyDescriptionResponse

instance ToPath UpdateKeyDescription where
    toPath = const "/"

instance ToQuery UpdateKeyDescription where
    toQuery = const mempty

instance ToHeaders UpdateKeyDescription

instance ToJSON UpdateKeyDescription where
    toJSON UpdateKeyDescription{..} = object
        [ "KeyId"       .= _ukdKeyId
        , "Description" .= _ukdDescription
        ]

json

instance AWSRequest UpdateKeyDescription where
    type Sv UpdateKeyDescription = KMS
    type Rs UpdateKeyDescription = UpdateKeyDescriptionResponse

    request  = post "UpdateKeyDescription"
    response = nullResponse UpdateKeyDescriptionResponse

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
import Network.AWS.Request
import Network.AWS.KMS.Types

data UpdateKeyDescription = UpdateKeyDescription
    { _ukdDescription :: Text
    , _ukdKeyId       :: Text
    } deriving (Eq, Ord, Show, Generic)

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

instance ToPath UpdateKeyDescription where
    toPath = const "/"

instance ToQuery UpdateKeyDescription where
    toQuery = const mempty

instance ToHeaders UpdateKeyDescription

instance ToBody UpdateKeyDescription where
    toBody = toBody . encode . _ukdKeyId

data UpdateKeyDescriptionResponse = UpdateKeyDescriptionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UpdateKeyDescriptionResponse' constructor.
updateKeyDescriptionResponse :: UpdateKeyDescriptionResponse
updateKeyDescriptionResponse = UpdateKeyDescriptionResponse

-- FromJSON

instance AWSRequest UpdateKeyDescription where
    type Sv UpdateKeyDescription = KMS
    type Rs UpdateKeyDescription = UpdateKeyDescriptionResponse

    request  = post'
    response = nullaryResponse UpdateKeyDescriptionResponse

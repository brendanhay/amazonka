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

-- Module      : Network.AWS.EC2.DeleteKeyPair
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified key pair, by removing the public key from Amazon EC2.
module Network.AWS.EC2.DeleteKeyPair
    (
    -- * Request
      DeleteKeyPair
    -- ** Request constructor
    , deleteKeyPair
    -- ** Request lenses
    , dkp1DryRun
    , dkp1KeyName

    -- * Response
    , DeleteKeyPairResponse
    -- ** Response constructor
    , deleteKeyPairResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DeleteKeyPair = DeleteKeyPair
    { _dkp1DryRun  :: Maybe Bool
    , _dkp1KeyName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteKeyPair' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dkp1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dkp1KeyName' @::@ 'Text'
--
deleteKeyPair :: Text -- ^ 'dkp1KeyName'
              -> DeleteKeyPair
deleteKeyPair p1 = DeleteKeyPair
    { _dkp1KeyName = p1
    , _dkp1DryRun  = Nothing
    }

dkp1DryRun :: Lens' DeleteKeyPair (Maybe Bool)
dkp1DryRun = lens _dkp1DryRun (\s a -> s { _dkp1DryRun = a })

-- | The name of the key pair.
dkp1KeyName :: Lens' DeleteKeyPair Text
dkp1KeyName = lens _dkp1KeyName (\s a -> s { _dkp1KeyName = a })

instance ToQuery DeleteKeyPair

instance ToPath DeleteKeyPair where
    toPath = const "/"

data DeleteKeyPairResponse = DeleteKeyPairResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteKeyPairResponse' constructor.
deleteKeyPairResponse :: DeleteKeyPairResponse
deleteKeyPairResponse = DeleteKeyPairResponse

instance FromXML DeleteKeyPairResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteKeyPairResponse"

instance AWSRequest DeleteKeyPair where
    type Sv DeleteKeyPair = EC2
    type Rs DeleteKeyPair = DeleteKeyPairResponse

    request  = post "DeleteKeyPair"
    response = nullaryResponse DeleteKeyPairResponse

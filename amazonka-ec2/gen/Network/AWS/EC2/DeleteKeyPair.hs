{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteKeyPair.html>
module Network.AWS.EC2.DeleteKeyPair
    (
    -- * Request
      DeleteKeyPair
    -- ** Request constructor
    , deleteKeyPair
    -- ** Request lenses
    , dkpDryRun
    , dkpKeyName

    -- * Response
    , DeleteKeyPairResponse
    -- ** Response constructor
    , deleteKeyPairResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DeleteKeyPair = DeleteKeyPair
    { _dkpDryRun  :: Maybe Bool
    , _dkpKeyName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteKeyPair' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dkpDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dkpKeyName' @::@ 'Text'
--
deleteKeyPair :: Text -- ^ 'dkpKeyName'
              -> DeleteKeyPair
deleteKeyPair p1 = DeleteKeyPair
    { _dkpKeyName = p1
    , _dkpDryRun  = Nothing
    }

dkpDryRun :: Lens' DeleteKeyPair (Maybe Bool)
dkpDryRun = lens _dkpDryRun (\s a -> s { _dkpDryRun = a })

-- | The name of the key pair.
dkpKeyName :: Lens' DeleteKeyPair Text
dkpKeyName = lens _dkpKeyName (\s a -> s { _dkpKeyName = a })

data DeleteKeyPairResponse = DeleteKeyPairResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteKeyPairResponse' constructor.
deleteKeyPairResponse :: DeleteKeyPairResponse
deleteKeyPairResponse = DeleteKeyPairResponse

instance ToPath DeleteKeyPair where
    toPath = const "/"

instance ToQuery DeleteKeyPair

instance ToHeaders DeleteKeyPair

instance AWSRequest DeleteKeyPair where
    type Sv DeleteKeyPair = EC2
    type Rs DeleteKeyPair = DeleteKeyPairResponse

    request  = post "DeleteKeyPair"
    response = nullResponse DeleteKeyPairResponse

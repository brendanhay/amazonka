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

-- Module      : Network.AWS.CloudTrail.DeleteTrail
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

-- | Deletes a trail.
--
-- <http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_DeleteTrail.html>
module Network.AWS.CloudTrail.DeleteTrail
    (
    -- * Request
      DeleteTrail
    -- ** Request constructor
    , deleteTrail
    -- ** Request lenses
    , dtName

    -- * Response
    , DeleteTrailResponse
    -- ** Response constructor
    , deleteTrailResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudTrail.Types
import qualified GHC.Exts

newtype DeleteTrail = DeleteTrail
    { _dtName :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DeleteTrail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtName' @::@ 'Text'
--
deleteTrail :: Text -- ^ 'dtName'
            -> DeleteTrail
deleteTrail p1 = DeleteTrail
    { _dtName = p1
    }

-- | The name of a trail to be deleted.
dtName :: Lens' DeleteTrail Text
dtName = lens _dtName (\s a -> s { _dtName = a })

data DeleteTrailResponse = DeleteTrailResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteTrailResponse' constructor.
deleteTrailResponse :: DeleteTrailResponse
deleteTrailResponse = DeleteTrailResponse

instance ToPath DeleteTrail where
    toPath = const "/"

instance ToQuery DeleteTrail where
    toQuery = const mempty

instance ToHeaders DeleteTrail

instance ToJSON DeleteTrail where
    toJSON DeleteTrail{..} = object
        [ "Name" .= _dtName
        ]

instance AWSRequest DeleteTrail where
    type Sv DeleteTrail = CloudTrail
    type Rs DeleteTrail = DeleteTrailResponse

    request  = post "DeleteTrail"
    response = nullResponse DeleteTrailResponse

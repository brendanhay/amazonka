{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudTrail.V2013_11_01.DeleteTrail
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a trail.
module Network.AWS.CloudTrail.V2013_11_01.DeleteTrail
    (
    -- * Request
      DeleteTrail
    -- ** Request constructor
    , mkDeleteTrail
    -- ** Request lenses
    , dtName

    -- * Response
    , DeleteTrailResponse
    -- ** Response constructor
    , mkDeleteTrailResponse
    ) where

import Network.AWS.CloudTrail.V2013_11_01.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The request that specifies the name of a trail to delete.
newtype DeleteTrail = DeleteTrail
    { _dtName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteTrail' request.
mkDeleteTrail :: Text -- ^ 'dtName'
              -> DeleteTrail
mkDeleteTrail p1 = DeleteTrail
    { _dtName = p1
    }

-- | The name of a trail to be deleted.
dtName :: Lens' DeleteTrail Text
dtName = lens _dtName (\s a -> s { _dtName = a })

instance ToPath DeleteTrail

instance ToQuery DeleteTrail

instance ToHeaders DeleteTrail

instance ToJSON DeleteTrail

-- | Returns the objects or data listed below if successful. Otherwise, returns
-- an error.
data DeleteTrailResponse = DeleteTrailResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteTrailResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteTrailResponse :: DeleteTrailResponse
mkDeleteTrailResponse = DeleteTrailResponse

instance AWSRequest DeleteTrail where
    type Sv DeleteTrail = CloudTrail
    type Rs DeleteTrail = DeleteTrailResponse

    request = get
    response _ = nullaryResponse DeleteTrailResponse

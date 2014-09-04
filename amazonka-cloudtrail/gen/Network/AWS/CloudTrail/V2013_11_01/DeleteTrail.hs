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
    , deleteTrail
    -- ** Request lenses
    , dtrName

    -- * Response
    , DeleteTrailResponse
    ) where

import           Network.AWS.CloudTrail.V2013_11_01.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DeleteTrail' request.
deleteTrail :: Text -- ^ 'dtrName'
            -> DeleteTrail
deleteTrail p1 = DeleteTrail
    { _dtrName = p1
    }
{-# INLINE deleteTrail #-}

data DeleteTrail = DeleteTrail
    { _dtrName :: Text
      -- ^ The name of a trail to be deleted.
    } deriving (Show, Generic)

-- | The name of a trail to be deleted.
dtrName :: Lens' DeleteTrail (Text)
dtrName f x =
    f (_dtrName x)
        <&> \y -> x { _dtrName = y }
{-# INLINE dtrName #-}

instance ToPath DeleteTrail

instance ToQuery DeleteTrail

instance ToHeaders DeleteTrail

instance ToJSON DeleteTrail

data DeleteTrailResponse = DeleteTrailResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteTrail where
    type Sv DeleteTrail = CloudTrail
    type Rs DeleteTrail = DeleteTrailResponse

    request = get
    response _ = nullaryResponse DeleteTrailResponse

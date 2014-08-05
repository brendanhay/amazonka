{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.CloudTrail.V2013_11_01.DeleteTrail where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CloudTrail.V2013_11_01.Types
import Network.AWS.Prelude

data DeleteTrail = DeleteTrail
    { _dttName :: Text
      -- ^ The name of a trail to be deleted.
    } deriving (Show, Generic)

makeLenses ''DeleteTrail

instance ToPath DeleteTrail

instance ToQuery DeleteTrail

instance ToHeaders DeleteTrail

instance ToJSON DeleteTrail

data DeleteTrailResponse = DeleteTrailResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteTrailResponse

instance AWSRequest DeleteTrail where
    type Sv DeleteTrail = CloudTrail
    type Rs DeleteTrail = DeleteTrailResponse

    request = get
    response _ _ = return (Right DeleteTrailResponse)

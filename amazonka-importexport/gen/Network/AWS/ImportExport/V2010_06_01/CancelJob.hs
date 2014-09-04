{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ImportExport.V2010_06_01.CancelJob
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation cancels a specified job. Only the job owner can cancel it.
-- The operation fails if the job has already started or is complete.
module Network.AWS.ImportExport.V2010_06_01.CancelJob
    (
    -- * Request
      CancelJob
    -- ** Request constructor
    , cancelJob
    -- ** Request lenses
    , cjiJobId

    -- * Response
    , CancelJobResponse
    -- ** Response lenses
    , cjoSuccess
    ) where

import Network.AWS.Request.Query
import Network.AWS.ImportExport.V2010_06_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CancelJob' request.
cancelJob :: Text -- ^ 'cjiJobId'
          -> CancelJob
cancelJob p1 = CancelJob
    { _cjiJobId = p1
    }
{-# INLINE cancelJob #-}

data CancelJob = CancelJob
    { _cjiJobId :: Text
      -- ^ A unique identifier which refers to a particular job.
    } deriving (Show, Generic)

-- | A unique identifier which refers to a particular job.
cjiJobId :: Lens' CancelJob (Text)
cjiJobId f x =
    f (_cjiJobId x)
        <&> \y -> x { _cjiJobId = y }
{-# INLINE cjiJobId #-}

instance ToQuery CancelJob where
    toQuery = genericQuery def

data CancelJobResponse = CancelJobResponse
    { _cjoSuccess :: Maybe Bool
      -- ^ Specifies whether (true) or not (false) AWS Import/Export updated
      -- your job.
    } deriving (Show, Generic)

-- | Specifies whether (true) or not (false) AWS Import/Export updated your job.
cjoSuccess :: Lens' CancelJobResponse (Maybe Bool)
cjoSuccess f x =
    f (_cjoSuccess x)
        <&> \y -> x { _cjoSuccess = y }
{-# INLINE cjoSuccess #-}

instance FromXML CancelJobResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CancelJob where
    type Sv CancelJob = ImportExport
    type Rs CancelJob = CancelJobResponse

    request = post "CancelJob"
    response _ = xmlResponse

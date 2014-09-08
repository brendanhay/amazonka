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
    , mkCancelJob
    -- ** Request lenses
    , cjJobId

    -- * Response
    , CancelJobResponse
    -- ** Response lenses
    , cjrSuccess
    ) where

import Network.AWS.Request.Query
import Network.AWS.ImportExport.V2010_06_01.Types
import Network.AWS.Prelude

-- | Input structure for the CancelJob operation.
newtype CancelJob = CancelJob
    { _cjJobId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CancelJob' request.
mkCancelJob :: Text -- ^ 'cjJobId'
            -> CancelJob
mkCancelJob p1 = CancelJob
    { _cjJobId = p1
    }

-- | A unique identifier which refers to a particular job.
cjJobId :: Lens' CancelJob Text
cjJobId = lens _cjJobId (\s a -> s { _cjJobId = a })

instance ToQuery CancelJob where
    toQuery = genericQuery def

-- | Output structure for the CancelJob operation.
newtype CancelJobResponse = CancelJobResponse
    { _cjrSuccess :: Maybe Bool
    } deriving (Show, Generic)

-- | Specifies whether (true) or not (false) AWS Import/Export updated your job.
cjrSuccess :: Lens' CancelJobResponse (Maybe Bool)
cjrSuccess = lens _cjrSuccess (\s a -> s { _cjrSuccess = a })

instance FromXML CancelJobResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CancelJob where
    type Sv CancelJob = ImportExport
    type Rs CancelJob = CancelJobResponse

    request = post "CancelJob"
    response _ = xmlResponse

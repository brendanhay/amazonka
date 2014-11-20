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

-- Module      : Network.AWS.ImportExport.CancelJob
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
--
-- <http://docs.aws.amazon.com/AWSImportExport/latest/DG/WebCancelJob.html>
module Network.AWS.ImportExport.CancelJob
    (
    -- * Request
      CancelJob
    -- ** Request constructor
    , cancelJob
    -- ** Request lenses
    , cjJobId

    -- * Response
    , CancelJobResponse
    -- ** Response constructor
    , cancelJobResponse
    -- ** Response lenses
    , cjrSuccess
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ImportExport.Types
import qualified GHC.Exts

newtype CancelJob = CancelJob
    { _cjJobId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'CancelJob' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cjJobId' @::@ 'Text'
--
cancelJob :: Text -- ^ 'cjJobId'
          -> CancelJob
cancelJob p1 = CancelJob
    { _cjJobId = p1
    }

cjJobId :: Lens' CancelJob Text
cjJobId = lens _cjJobId (\s a -> s { _cjJobId = a })

newtype CancelJobResponse = CancelJobResponse
    { _cjrSuccess :: Maybe Bool
    } deriving (Eq, Ord, Show)

-- | 'CancelJobResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cjrSuccess' @::@ 'Maybe' 'Bool'
--
cancelJobResponse :: CancelJobResponse
cancelJobResponse = CancelJobResponse
    { _cjrSuccess = Nothing
    }

cjrSuccess :: Lens' CancelJobResponse (Maybe Bool)
cjrSuccess = lens _cjrSuccess (\s a -> s { _cjrSuccess = a })

instance ToPath CancelJob where
    toPath = const "/"

instance ToQuery CancelJob where
    toQuery CancelJob{..} = mconcat
        [ "JobId" =? _cjJobId
        ]

instance ToHeaders CancelJob

query

instance AWSRequest CancelJob where
    type Sv CancelJob = ImportExport
    type Rs CancelJob = CancelJobResponse

    request  = post "CancelJob"
    response = xmlResponse

instance FromXML CancelJobResponse where
    parseXML = withElement "CancelJobResult" $ \x -> CancelJobResponse
        <$> x .@? "Success"

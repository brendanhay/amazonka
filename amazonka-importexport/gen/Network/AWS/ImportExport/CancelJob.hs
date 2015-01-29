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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation cancels a specified job. Only the job owner can cancel it. The
-- operation fails if the job has already started or is complete.
--
-- <http://docs.aws.amazon.com/AWSImportExport/latest/DG/WebCancelJob.html>
module Network.AWS.ImportExport.CancelJob
    (
    -- * Request
      CancelJob
    -- ** Request constructor
    , cancelJob
    -- ** Request lenses
    , cj1APIVersion
    , cj1JobId

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

data CancelJob = CancelJob
    { _cj1APIVersion :: Maybe Text
    , _cj1JobId      :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CancelJob' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cj1APIVersion' @::@ 'Maybe' 'Text'
--
-- * 'cj1JobId' @::@ 'Text'
--
cancelJob :: Text -- ^ 'cj1JobId'
          -> CancelJob
cancelJob p1 = CancelJob
    { _cj1JobId      = p1
    , _cj1APIVersion = Nothing
    }

cj1APIVersion :: Lens' CancelJob (Maybe Text)
cj1APIVersion = lens _cj1APIVersion (\s a -> s { _cj1APIVersion = a })

cj1JobId :: Lens' CancelJob Text
cj1JobId = lens _cj1JobId (\s a -> s { _cj1JobId = a })

newtype CancelJobResponse = CancelJobResponse
    { _cjrSuccess :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

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
        [ "APIVersion" =? _cj1APIVersion
        , "JobId"      =? _cj1JobId
        ]

instance ToHeaders CancelJob

instance AWSRequest CancelJob where
    type Sv CancelJob = ImportExport
    type Rs CancelJob = CancelJobResponse

    request  = post "CancelJob"
    response = xmlResponse

instance FromXML CancelJobResponse where
    parseXML = withElement "CancelJobResult" $ \x -> CancelJobResponse
        <$> x .@? "Success"

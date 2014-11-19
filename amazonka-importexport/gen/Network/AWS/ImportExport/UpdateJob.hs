{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ImportExport.UpdateJob
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | You use this operation to change the parameters specified in the original
-- manifest file by supplying a new manifest file. The manifest file attached
-- to this request replaces the original manifest file. You can only use the
-- operation after a CreateJob request but before the data transfer starts and
-- you can only use it on jobs you own.
--
-- <http://docs.aws.amazon.com/AWSImportExport/latest/DG/WebUpdateJob.html>
module Network.AWS.ImportExport.UpdateJob
    (
    -- * Request
      UpdateJob
    -- ** Request constructor
    , updateJob
    -- ** Request lenses
    , ujJobId
    , ujJobType
    , ujManifest
    , ujValidateOnly

    -- * Response
    , UpdateJobResponse
    -- ** Response constructor
    , updateJobResponse
    -- ** Response lenses
    , ujrSuccess
    , ujrWarningMessage
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ImportExport.Types
import qualified GHC.Exts

data UpdateJob = UpdateJob
    { _ujJobId        :: Text
    , _ujJobType      :: Text
    , _ujManifest     :: Text
    , _ujValidateOnly :: Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'UpdateJob' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ujJobId' @::@ 'Text'
--
-- * 'ujJobType' @::@ 'Text'
--
-- * 'ujManifest' @::@ 'Text'
--
-- * 'ujValidateOnly' @::@ 'Bool'
--
updateJob :: Text -- ^ 'ujJobId'
          -> Text -- ^ 'ujManifest'
          -> Text -- ^ 'ujJobType'
          -> Bool -- ^ 'ujValidateOnly'
          -> UpdateJob
updateJob p1 p2 p3 p4 = UpdateJob
    { _ujJobId        = p1
    , _ujManifest     = p2
    , _ujJobType      = p3
    , _ujValidateOnly = p4
    }

ujJobId :: Lens' UpdateJob Text
ujJobId = lens _ujJobId (\s a -> s { _ujJobId = a })

ujJobType :: Lens' UpdateJob Text
ujJobType = lens _ujJobType (\s a -> s { _ujJobType = a })

ujManifest :: Lens' UpdateJob Text
ujManifest = lens _ujManifest (\s a -> s { _ujManifest = a })

ujValidateOnly :: Lens' UpdateJob Bool
ujValidateOnly = lens _ujValidateOnly (\s a -> s { _ujValidateOnly = a })

data UpdateJobResponse = UpdateJobResponse
    { _ujrSuccess        :: Maybe Bool
    , _ujrWarningMessage :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UpdateJobResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ujrSuccess' @::@ 'Maybe' 'Bool'
--
-- * 'ujrWarningMessage' @::@ 'Maybe' 'Text'
--
updateJobResponse :: UpdateJobResponse
updateJobResponse = UpdateJobResponse
    { _ujrSuccess        = Nothing
    , _ujrWarningMessage = Nothing
    }

ujrSuccess :: Lens' UpdateJobResponse (Maybe Bool)
ujrSuccess = lens _ujrSuccess (\s a -> s { _ujrSuccess = a })

ujrWarningMessage :: Lens' UpdateJobResponse (Maybe Text)
ujrWarningMessage =
    lens _ujrWarningMessage (\s a -> s { _ujrWarningMessage = a })

instance ToPath UpdateJob where
    toPath = const "/"

instance ToQuery UpdateJob

instance ToHeaders UpdateJob

instance AWSRequest UpdateJob where
    type Sv UpdateJob = ImportExport
    type Rs UpdateJob = UpdateJobResponse

    request  = post "UpdateJob"
    response = xmlResponse

instance FromXML UpdateJobResponse where
    parseXML = withElement "UpdateJobResult" $ \x ->
            <$> x .@? "Success"
            <*> x .@? "WarningMessage"

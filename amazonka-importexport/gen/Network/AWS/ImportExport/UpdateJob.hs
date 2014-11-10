{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.ImportExport.UpdateJob
    (
    -- * Request
      UpdateJobInput
    -- ** Request constructor
    , updateJob
    -- ** Request lenses
    , ujiJobId
    , ujiJobType
    , ujiManifest
    , ujiValidateOnly

    -- * Response
    , UpdateJobOutput
    -- ** Response constructor
    , updateJobResponse
    -- ** Response lenses
    , ujoSuccess
    , ujoWarningMessage
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ImportExport.Types

data UpdateJobInput = UpdateJobInput
    { _ujiJobId        :: Text
    , _ujiJobType      :: Text
    , _ujiManifest     :: Text
    , _ujiValidateOnly :: Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'UpdateJobInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ujiJobId' @::@ 'Text'
--
-- * 'ujiJobType' @::@ 'Text'
--
-- * 'ujiManifest' @::@ 'Text'
--
-- * 'ujiValidateOnly' @::@ 'Bool'
--
updateJob :: Text -- ^ 'ujiJobId'
          -> Text -- ^ 'ujiManifest'
          -> Text -- ^ 'ujiJobType'
          -> Bool -- ^ 'ujiValidateOnly'
          -> UpdateJobInput
updateJob p1 p2 p3 p4 = UpdateJobInput
    { _ujiJobId        = p1
    , _ujiManifest     = p2
    , _ujiJobType      = p3
    , _ujiValidateOnly = p4
    }

ujiJobId :: Lens' UpdateJobInput Text
ujiJobId = lens _ujiJobId (\s a -> s { _ujiJobId = a })

ujiJobType :: Lens' UpdateJobInput Text
ujiJobType = lens _ujiJobType (\s a -> s { _ujiJobType = a })

ujiManifest :: Lens' UpdateJobInput Text
ujiManifest = lens _ujiManifest (\s a -> s { _ujiManifest = a })

ujiValidateOnly :: Lens' UpdateJobInput Bool
ujiValidateOnly = lens _ujiValidateOnly (\s a -> s { _ujiValidateOnly = a })

instance ToPath UpdateJobInput where
    toPath = const "/"

instance ToQuery UpdateJobInput

data UpdateJobOutput = UpdateJobOutput
    { _ujoSuccess        :: Maybe Bool
    , _ujoWarningMessage :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UpdateJobOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ujoSuccess' @::@ 'Maybe' 'Bool'
--
-- * 'ujoWarningMessage' @::@ 'Maybe' 'Text'
--
updateJobResponse :: UpdateJobOutput
updateJobResponse = UpdateJobOutput
    { _ujoSuccess        = Nothing
    , _ujoWarningMessage = Nothing
    }

ujoSuccess :: Lens' UpdateJobOutput (Maybe Bool)
ujoSuccess = lens _ujoSuccess (\s a -> s { _ujoSuccess = a })

ujoWarningMessage :: Lens' UpdateJobOutput (Maybe Text)
ujoWarningMessage =
    lens _ujoWarningMessage (\s a -> s { _ujoWarningMessage = a })

instance AWSRequest UpdateJobInput where
    type Sv UpdateJobInput = ImportExport
    type Rs UpdateJobInput = UpdateJobOutput

    request  = post "UpdateJob"
    response = xmlResponse $ \h x -> UpdateJobOutput
        <$> x %| "Success"
        <*> x %| "WarningMessage"

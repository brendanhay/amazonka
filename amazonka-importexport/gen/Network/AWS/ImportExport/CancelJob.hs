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
module Network.AWS.ImportExport.CancelJob
    (
    -- * Request
      CancelJobInput
    -- ** Request constructor
    , cancelJobInput
    -- ** Request lenses
    , cjiJobId

    -- * Response
    , CancelJobOutput
    -- ** Response constructor
    , cancelJobOutput
    -- ** Response lenses
    , cjoSuccess
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ImportExport.Types

newtype CancelJobInput = CancelJobInput
    { _cjiJobId :: Text
    } (Eq, Ord, Show, Generic, Monoid)

-- | 'CancelJobInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cjiJobId' @::@ 'Text'
--
cancelJobInput :: Text -- ^ 'cjiJobId'
               -> CancelJobInput
cancelJobInput p1 = CancelJobInput
    { _cjiJobId = p1
    }

cjiJobId :: Lens' CancelJobInput Text
cjiJobId = lens _cjiJobId (\s a -> s { _cjiJobId = a })
instance ToQuery CancelJobInput

instance ToPath CancelJobInput where
    toPath = const "/"

newtype CancelJobOutput = CancelJobOutput
    { _cjoSuccess :: Maybe Bool
    } (Eq, Ord, Show, Generic)

-- | 'CancelJobOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cjoSuccess' @::@ 'Maybe' 'Bool'
--
cancelJobOutput :: CancelJobOutput
cancelJobOutput = CancelJobOutput
    { _cjoSuccess = Nothing
    }

cjoSuccess :: Lens' CancelJobOutput (Maybe Bool)
cjoSuccess = lens _cjoSuccess (\s a -> s { _cjoSuccess = a })

instance FromXML CancelJobOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CancelJobOutput"

instance AWSRequest CancelJobInput where
    type Sv CancelJobInput = ImportExport
    type Rs CancelJobInput = CancelJobOutput

    request  = post "CancelJob"
    response = xmlResponse $ \h x -> CancelJobOutput
        <$> x %| "Success"

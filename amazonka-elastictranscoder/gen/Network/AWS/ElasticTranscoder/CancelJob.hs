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

-- Module      : Network.AWS.ElasticTranscoder.CancelJob
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CancelJob operation cancels an unfinished job.
--
-- <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/CancelJob.html>
module Network.AWS.ElasticTranscoder.CancelJob
    (
    -- * Request
      CancelJob
    -- ** Request constructor
    , cancelJob
    -- ** Request lenses
    , cjId

    -- * Response
    , CancelJobResponse
    -- ** Response constructor
    , cancelJobResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.ElasticTranscoder.Types
import qualified GHC.Exts

newtype CancelJob = CancelJob
    { _cjId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'CancelJob' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cjId' @::@ 'Text'
--
cancelJob :: Text -- ^ 'cjId'
          -> CancelJob
cancelJob p1 = CancelJob
    { _cjId = p1
    }

-- | The identifier of the job that you want to cancel. To get a list of the
-- jobs (including their jobId) that have a status of Submitted, use the
-- ListJobsByStatus API action.
cjId :: Lens' CancelJob Text
cjId = lens _cjId (\s a -> s { _cjId = a })

data CancelJobResponse = CancelJobResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CancelJobResponse' constructor.
cancelJobResponse :: CancelJobResponse
cancelJobResponse = CancelJobResponse

instance ToPath CancelJob where
    toPath CancelJob{..} = mconcat
        [ "/2012-09-25/jobs/"
        , toText _cjId
        ]

instance ToQuery CancelJob where
    toQuery = const mempty

instance ToHeaders CancelJob

instance ToJSON CancelJob where
    toJSON = const Null

instance AWSRequest CancelJob where
    type Sv CancelJob = ElasticTranscoder
    type Rs CancelJob = CancelJobResponse

    request  = delete
    response = nullResponse CancelJobResponse

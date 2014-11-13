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

-- Module      : Network.AWS.ElasticTranscoder.DeletePipeline
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeletePipeline operation removes a pipeline. You can only delete a
-- pipeline that has never been used or that is not currently in use (doesn't
-- contain any active jobs). If the pipeline is currently in use,
-- DeletePipeline returns an error.
module Network.AWS.ElasticTranscoder.DeletePipeline
    (
    -- * Request
      DeletePipeline
    -- ** Request constructor
    , deletePipeline
    -- ** Request lenses
    , dp1Id

    -- * Response
    , DeletePipelineResponse
    -- ** Response constructor
    , deletePipelineResponse
    ) where

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ElasticTranscoder.Types

newtype DeletePipeline = DeletePipeline
    { _dp1Id :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeletePipeline' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dp1Id' @::@ 'Text'
--
deletePipeline :: Text -- ^ 'dp1Id'
               -> DeletePipeline
deletePipeline p1 = DeletePipeline
    { _dp1Id = p1
    }

-- | The identifier of the pipeline that you want to delete.
dp1Id :: Lens' DeletePipeline Text
dp1Id = lens _dp1Id (\s a -> s { _dp1Id = a })

instance ToPath DeletePipeline where
    toPath DeletePipeline{..} = mconcat
        [ "/2012-09-25/pipelines/"
        , toText _dp1Id
        ]

instance ToQuery DeletePipeline where
    toQuery = const mempty

instance ToHeaders DeletePipeline

data DeletePipelineResponse = DeletePipelineResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeletePipelineResponse' constructor.
deletePipelineResponse :: DeletePipelineResponse
deletePipelineResponse = DeletePipelineResponse

-- FromJSON

instance AWSRequest DeletePipeline where
    type Sv DeletePipeline = ElasticTranscoder
    type Rs DeletePipeline = DeletePipelineResponse

    request  = delete'
    response = nullaryResponse DeletePipelineResponse

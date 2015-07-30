{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.DeletePipeline
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified pipeline.
--
-- <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_DeletePipeline.html>
module Network.AWS.CodePipeline.DeletePipeline
    (
    -- * Request
      DeletePipeline
    -- ** Request constructor
    , deletePipeline
    -- ** Request lenses
    , dpName

    -- * Response
    , DeletePipelineResponse
    -- ** Response constructor
    , deletePipelineResponse
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a delete pipeline action.
--
-- /See:/ 'deletePipeline' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpName'
newtype DeletePipeline = DeletePipeline'
    { _dpName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeletePipeline' smart constructor.
deletePipeline :: Text -> DeletePipeline
deletePipeline pName_ =
    DeletePipeline'
    { _dpName = pName_
    }

-- | The name of the pipeline to be deleted.
dpName :: Lens' DeletePipeline Text
dpName = lens _dpName (\ s a -> s{_dpName = a});

instance AWSRequest DeletePipeline where
        type Sv DeletePipeline = CodePipeline
        type Rs DeletePipeline = DeletePipelineResponse
        request = postJSON
        response = receiveNull DeletePipelineResponse'

instance ToHeaders DeletePipeline where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.DeletePipeline" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeletePipeline where
        toJSON DeletePipeline'{..}
          = object ["name" .= _dpName]

instance ToPath DeletePipeline where
        toPath = const "/"

instance ToQuery DeletePipeline where
        toQuery = const mempty

-- | /See:/ 'deletePipelineResponse' smart constructor.
data DeletePipelineResponse =
    DeletePipelineResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeletePipelineResponse' smart constructor.
deletePipelineResponse :: DeletePipelineResponse
deletePipelineResponse = DeletePipelineResponse'

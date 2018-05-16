{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.DeletePipeline
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified pipeline.
--
--
module Network.AWS.CodePipeline.DeletePipeline
    (
    -- * Creating a Request
      deletePipeline
    , DeletePipeline
    -- * Request Lenses
    , dpName

    -- * Destructuring the Response
    , deletePipelineResponse
    , DeletePipelineResponse
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a DeletePipeline action.
--
--
--
-- /See:/ 'deletePipeline' smart constructor.
newtype DeletePipeline = DeletePipeline'
  { _dpName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePipeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpName' - The name of the pipeline to be deleted.
deletePipeline
    :: Text -- ^ 'dpName'
    -> DeletePipeline
deletePipeline pName_ = DeletePipeline' {_dpName = pName_}


-- | The name of the pipeline to be deleted.
dpName :: Lens' DeletePipeline Text
dpName = lens _dpName (\ s a -> s{_dpName = a})

instance AWSRequest DeletePipeline where
        type Rs DeletePipeline = DeletePipelineResponse
        request = postJSON codePipeline
        response = receiveNull DeletePipelineResponse'

instance Hashable DeletePipeline where

instance NFData DeletePipeline where

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
          = object (catMaybes [Just ("name" .= _dpName)])

instance ToPath DeletePipeline where
        toPath = const "/"

instance ToQuery DeletePipeline where
        toQuery = const mempty

-- | /See:/ 'deletePipelineResponse' smart constructor.
data DeletePipelineResponse =
  DeletePipelineResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePipelineResponse' with the minimum fields required to make a request.
--
deletePipelineResponse
    :: DeletePipelineResponse
deletePipelineResponse = DeletePipelineResponse'


instance NFData DeletePipelineResponse where

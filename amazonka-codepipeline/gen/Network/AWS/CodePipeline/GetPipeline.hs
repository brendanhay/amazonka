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
-- Module      : Network.AWS.CodePipeline.GetPipeline
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the metadata, structure, stages, and actions of a pipeline. Can be used to return the entire structure of a pipeline in JSON format, which can then be modified and used to update the pipeline structure with 'UpdatePipeline' .
--
--
module Network.AWS.CodePipeline.GetPipeline
    (
    -- * Creating a Request
      getPipeline
    , GetPipeline
    -- * Request Lenses
    , gpVersion
    , gpName

    -- * Destructuring the Response
    , getPipelineResponse
    , GetPipelineResponse
    -- * Response Lenses
    , gprsPipeline
    , gprsMetadata
    , gprsResponseStatus
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a GetPipeline action.
--
--
--
-- /See:/ 'getPipeline' smart constructor.
data GetPipeline = GetPipeline'
  { _gpVersion :: !(Maybe Nat)
  , _gpName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPipeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpVersion' - The version number of the pipeline. If you do not specify a version, defaults to the most current version.
--
-- * 'gpName' - The name of the pipeline for which you want to get information. Pipeline names must be unique under an Amazon Web Services (AWS) user account.
getPipeline
    :: Text -- ^ 'gpName'
    -> GetPipeline
getPipeline pName_ = GetPipeline' {_gpVersion = Nothing, _gpName = pName_}


-- | The version number of the pipeline. If you do not specify a version, defaults to the most current version.
gpVersion :: Lens' GetPipeline (Maybe Natural)
gpVersion = lens _gpVersion (\ s a -> s{_gpVersion = a}) . mapping _Nat

-- | The name of the pipeline for which you want to get information. Pipeline names must be unique under an Amazon Web Services (AWS) user account.
gpName :: Lens' GetPipeline Text
gpName = lens _gpName (\ s a -> s{_gpName = a})

instance AWSRequest GetPipeline where
        type Rs GetPipeline = GetPipelineResponse
        request = postJSON codePipeline
        response
          = receiveJSON
              (\ s h x ->
                 GetPipelineResponse' <$>
                   (x .?> "pipeline") <*> (x .?> "metadata") <*>
                     (pure (fromEnum s)))

instance Hashable GetPipeline where

instance NFData GetPipeline where

instance ToHeaders GetPipeline where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.GetPipeline" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetPipeline where
        toJSON GetPipeline'{..}
          = object
              (catMaybes
                 [("version" .=) <$> _gpVersion,
                  Just ("name" .= _gpName)])

instance ToPath GetPipeline where
        toPath = const "/"

instance ToQuery GetPipeline where
        toQuery = const mempty

-- | Represents the output of a GetPipeline action.
--
--
--
-- /See:/ 'getPipelineResponse' smart constructor.
data GetPipelineResponse = GetPipelineResponse'
  { _gprsPipeline       :: !(Maybe PipelineDeclaration)
  , _gprsMetadata       :: !(Maybe PipelineMetadata)
  , _gprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPipelineResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gprsPipeline' - Represents the structure of actions and stages to be performed in the pipeline.
--
-- * 'gprsMetadata' - Represents the pipeline metadata information returned as part of the output of a GetPipeline action.
--
-- * 'gprsResponseStatus' - -- | The response status code.
getPipelineResponse
    :: Int -- ^ 'gprsResponseStatus'
    -> GetPipelineResponse
getPipelineResponse pResponseStatus_ =
  GetPipelineResponse'
    { _gprsPipeline = Nothing
    , _gprsMetadata = Nothing
    , _gprsResponseStatus = pResponseStatus_
    }


-- | Represents the structure of actions and stages to be performed in the pipeline.
gprsPipeline :: Lens' GetPipelineResponse (Maybe PipelineDeclaration)
gprsPipeline = lens _gprsPipeline (\ s a -> s{_gprsPipeline = a})

-- | Represents the pipeline metadata information returned as part of the output of a GetPipeline action.
gprsMetadata :: Lens' GetPipelineResponse (Maybe PipelineMetadata)
gprsMetadata = lens _gprsMetadata (\ s a -> s{_gprsMetadata = a})

-- | -- | The response status code.
gprsResponseStatus :: Lens' GetPipelineResponse Int
gprsResponseStatus = lens _gprsResponseStatus (\ s a -> s{_gprsResponseStatus = a})

instance NFData GetPipelineResponse where

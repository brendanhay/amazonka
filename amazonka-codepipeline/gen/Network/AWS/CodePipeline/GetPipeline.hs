{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.GetPipeline
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the metadata, structure, stages, and actions of a pipeline. Can
-- be used to return the entire structure of a pipeline in JSON format,
-- which can then be modified and used to update the pipeline structure
-- with UpdatePipeline.
--
-- <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_GetPipeline.html>
module Network.AWS.CodePipeline.GetPipeline
    (
    -- * Request
      GetPipeline
    -- ** Request constructor
    , getPipeline
    -- ** Request lenses
    , gpVersion
    , gpName

    -- * Response
    , GetPipelineResponse
    -- ** Response constructor
    , getPipelineResponse
    -- ** Response lenses
    , gprsPipeline
    , gprsStatus
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a get pipeline action.
--
-- /See:/ 'getPipeline' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpVersion'
--
-- * 'gpName'
data GetPipeline = GetPipeline'
    { _gpVersion :: !(Maybe Nat)
    , _gpName    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetPipeline' smart constructor.
getPipeline :: Text -> GetPipeline
getPipeline pName_ =
    GetPipeline'
    { _gpVersion = Nothing
    , _gpName = pName_
    }

-- | The version number of the pipeline. If you do not specify a version,
-- defaults to the most current version.
gpVersion :: Lens' GetPipeline (Maybe Natural)
gpVersion = lens _gpVersion (\ s a -> s{_gpVersion = a}) . mapping _Nat;

-- | The name of the pipeline for which you want to get information. Pipeline
-- names must be unique under an Amazon Web Services (AWS) user account.
gpName :: Lens' GetPipeline Text
gpName = lens _gpName (\ s a -> s{_gpName = a});

instance AWSRequest GetPipeline where
        type Sv GetPipeline = CodePipeline
        type Rs GetPipeline = GetPipelineResponse
        request = postJSON "GetPipeline"
        response
          = receiveJSON
              (\ s h x ->
                 GetPipelineResponse' <$>
                   (x .?> "pipeline") <*> (pure (fromEnum s)))

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
          = object ["version" .= _gpVersion, "name" .= _gpName]

instance ToPath GetPipeline where
        toPath = const "/"

instance ToQuery GetPipeline where
        toQuery = const mempty

-- | Represents the output of a get pipeline action.
--
-- /See:/ 'getPipelineResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gprsPipeline'
--
-- * 'gprsStatus'
data GetPipelineResponse = GetPipelineResponse'
    { _gprsPipeline :: !(Maybe PipelineDeclaration)
    , _gprsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetPipelineResponse' smart constructor.
getPipelineResponse :: Int -> GetPipelineResponse
getPipelineResponse pStatus_ =
    GetPipelineResponse'
    { _gprsPipeline = Nothing
    , _gprsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
gprsPipeline :: Lens' GetPipelineResponse (Maybe PipelineDeclaration)
gprsPipeline = lens _gprsPipeline (\ s a -> s{_gprsPipeline = a});

-- | FIXME: Undocumented member.
gprsStatus :: Lens' GetPipelineResponse Int
gprsStatus = lens _gprsStatus (\ s a -> s{_gprsStatus = a});

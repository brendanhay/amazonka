{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.CreateCustomActionType
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom action that can be used in all pipelines associated
-- with the AWS account. Only used for custom actions.
--
-- <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_CreateCustomActionType.html>
module Network.AWS.CodePipeline.CreateCustomActionType
    (
    -- * Request
      CreateCustomActionType
    -- ** Request constructor
    , createCustomActionType
    -- ** Request lenses
    , ccatrqSettings
    , ccatrqConfigurationProperties
    , ccatrqCategory
    , ccatrqProvider
    , ccatrqVersion
    , ccatrqInputArtifactDetails
    , ccatrqOutputArtifactDetails

    -- * Response
    , CreateCustomActionTypeResponse
    -- ** Response constructor
    , createCustomActionTypeResponse
    -- ** Response lenses
    , ccatrsStatus
    , ccatrsActionType
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a create custom action operation.
--
-- /See:/ 'createCustomActionType' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccatrqSettings'
--
-- * 'ccatrqConfigurationProperties'
--
-- * 'ccatrqCategory'
--
-- * 'ccatrqProvider'
--
-- * 'ccatrqVersion'
--
-- * 'ccatrqInputArtifactDetails'
--
-- * 'ccatrqOutputArtifactDetails'
data CreateCustomActionType = CreateCustomActionType'
    { _ccatrqSettings                :: !(Maybe ActionTypeSettings)
    , _ccatrqConfigurationProperties :: !(Maybe [ActionConfigurationProperty])
    , _ccatrqCategory                :: !ActionCategory
    , _ccatrqProvider                :: !Text
    , _ccatrqVersion                 :: !Text
    , _ccatrqInputArtifactDetails    :: !ArtifactDetails
    , _ccatrqOutputArtifactDetails   :: !ArtifactDetails
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateCustomActionType' smart constructor.
createCustomActionType :: ActionCategory -> Text -> Text -> ArtifactDetails -> ArtifactDetails -> CreateCustomActionType
createCustomActionType pCategory pProvider pVersion pInputArtifactDetails pOutputArtifactDetails =
    CreateCustomActionType'
    { _ccatrqSettings = Nothing
    , _ccatrqConfigurationProperties = Nothing
    , _ccatrqCategory = pCategory
    , _ccatrqProvider = pProvider
    , _ccatrqVersion = pVersion
    , _ccatrqInputArtifactDetails = pInputArtifactDetails
    , _ccatrqOutputArtifactDetails = pOutputArtifactDetails
    }

-- | FIXME: Undocumented member.
ccatrqSettings :: Lens' CreateCustomActionType (Maybe ActionTypeSettings)
ccatrqSettings = lens _ccatrqSettings (\ s a -> s{_ccatrqSettings = a});

-- | The configuration properties for the custom action.
ccatrqConfigurationProperties :: Lens' CreateCustomActionType [ActionConfigurationProperty]
ccatrqConfigurationProperties = lens _ccatrqConfigurationProperties (\ s a -> s{_ccatrqConfigurationProperties = a}) . _Default;

-- | The category of the custom action, such as a source action or a build
-- action.
ccatrqCategory :: Lens' CreateCustomActionType ActionCategory
ccatrqCategory = lens _ccatrqCategory (\ s a -> s{_ccatrqCategory = a});

-- | The provider of the service used in the custom action, such as AWS
-- CodeDeploy.
ccatrqProvider :: Lens' CreateCustomActionType Text
ccatrqProvider = lens _ccatrqProvider (\ s a -> s{_ccatrqProvider = a});

-- | The version number of the custom action.
--
-- A newly-created custom action is always assigned a version number of
-- @1@. This is required.
ccatrqVersion :: Lens' CreateCustomActionType Text
ccatrqVersion = lens _ccatrqVersion (\ s a -> s{_ccatrqVersion = a});

-- | FIXME: Undocumented member.
ccatrqInputArtifactDetails :: Lens' CreateCustomActionType ArtifactDetails
ccatrqInputArtifactDetails = lens _ccatrqInputArtifactDetails (\ s a -> s{_ccatrqInputArtifactDetails = a});

-- | FIXME: Undocumented member.
ccatrqOutputArtifactDetails :: Lens' CreateCustomActionType ArtifactDetails
ccatrqOutputArtifactDetails = lens _ccatrqOutputArtifactDetails (\ s a -> s{_ccatrqOutputArtifactDetails = a});

instance AWSRequest CreateCustomActionType where
        type Sv CreateCustomActionType = CodePipeline
        type Rs CreateCustomActionType =
             CreateCustomActionTypeResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateCustomActionTypeResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "actionType"))

instance ToHeaders CreateCustomActionType where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.CreateCustomActionType" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateCustomActionType where
        toJSON CreateCustomActionType'{..}
          = object
              ["settings" .= _ccatrqSettings,
               "configurationProperties" .=
                 _ccatrqConfigurationProperties,
               "category" .= _ccatrqCategory,
               "provider" .= _ccatrqProvider,
               "version" .= _ccatrqVersion,
               "inputArtifactDetails" .=
                 _ccatrqInputArtifactDetails,
               "outputArtifactDetails" .=
                 _ccatrqOutputArtifactDetails]

instance ToPath CreateCustomActionType where
        toPath = const "/"

instance ToQuery CreateCustomActionType where
        toQuery = const mempty

-- | Represents the output of a create custom action operation.
--
-- /See:/ 'createCustomActionTypeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccatrsStatus'
--
-- * 'ccatrsActionType'
data CreateCustomActionTypeResponse = CreateCustomActionTypeResponse'
    { _ccatrsStatus     :: !Int
    , _ccatrsActionType :: !ActionType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateCustomActionTypeResponse' smart constructor.
createCustomActionTypeResponse :: Int -> ActionType -> CreateCustomActionTypeResponse
createCustomActionTypeResponse pStatus pActionType =
    CreateCustomActionTypeResponse'
    { _ccatrsStatus = pStatus
    , _ccatrsActionType = pActionType
    }

-- | FIXME: Undocumented member.
ccatrsStatus :: Lens' CreateCustomActionTypeResponse Int
ccatrsStatus = lens _ccatrsStatus (\ s a -> s{_ccatrsStatus = a});

-- | FIXME: Undocumented member.
ccatrsActionType :: Lens' CreateCustomActionTypeResponse ActionType
ccatrsActionType = lens _ccatrsActionType (\ s a -> s{_ccatrsActionType = a});

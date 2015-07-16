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
    , ccatSettings
    , ccatConfigurationProperties
    , ccatCategory
    , ccatProvider
    , ccatVersion
    , ccatInputArtifactDetails
    , ccatOutputArtifactDetails

    -- * Response
    , CreateCustomActionTypeResponse
    -- ** Response constructor
    , createCustomActionTypeResponse
    -- ** Response lenses
    , ccatrStatus
    , ccatrActionType
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
-- * 'ccatSettings'
--
-- * 'ccatConfigurationProperties'
--
-- * 'ccatCategory'
--
-- * 'ccatProvider'
--
-- * 'ccatVersion'
--
-- * 'ccatInputArtifactDetails'
--
-- * 'ccatOutputArtifactDetails'
data CreateCustomActionType = CreateCustomActionType'
    { _ccatSettings                :: !(Maybe ActionTypeSettings)
    , _ccatConfigurationProperties :: !(Maybe [ActionConfigurationProperty])
    , _ccatCategory                :: !ActionCategory
    , _ccatProvider                :: !Text
    , _ccatVersion                 :: !Text
    , _ccatInputArtifactDetails    :: !ArtifactDetails
    , _ccatOutputArtifactDetails   :: !ArtifactDetails
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateCustomActionType' smart constructor.
createCustomActionType :: ActionCategory -> Text -> Text -> ArtifactDetails -> ArtifactDetails -> CreateCustomActionType
createCustomActionType pCategory pProvider pVersion pInputArtifactDetails pOutputArtifactDetails =
    CreateCustomActionType'
    { _ccatSettings = Nothing
    , _ccatConfigurationProperties = Nothing
    , _ccatCategory = pCategory
    , _ccatProvider = pProvider
    , _ccatVersion = pVersion
    , _ccatInputArtifactDetails = pInputArtifactDetails
    , _ccatOutputArtifactDetails = pOutputArtifactDetails
    }

-- | FIXME: Undocumented member.
ccatSettings :: Lens' CreateCustomActionType (Maybe ActionTypeSettings)
ccatSettings = lens _ccatSettings (\ s a -> s{_ccatSettings = a});

-- | The configuration properties for the custom action.
ccatConfigurationProperties :: Lens' CreateCustomActionType [ActionConfigurationProperty]
ccatConfigurationProperties = lens _ccatConfigurationProperties (\ s a -> s{_ccatConfigurationProperties = a}) . _Default;

-- | The category of the custom action, such as a source action or a build
-- action.
ccatCategory :: Lens' CreateCustomActionType ActionCategory
ccatCategory = lens _ccatCategory (\ s a -> s{_ccatCategory = a});

-- | The provider of the service used in the custom action, such as AWS
-- CodeDeploy.
ccatProvider :: Lens' CreateCustomActionType Text
ccatProvider = lens _ccatProvider (\ s a -> s{_ccatProvider = a});

-- | The version number of the custom action.
--
-- A newly-created custom action is always assigned a version number of
-- @1@. This is required.
ccatVersion :: Lens' CreateCustomActionType Text
ccatVersion = lens _ccatVersion (\ s a -> s{_ccatVersion = a});

-- | FIXME: Undocumented member.
ccatInputArtifactDetails :: Lens' CreateCustomActionType ArtifactDetails
ccatInputArtifactDetails = lens _ccatInputArtifactDetails (\ s a -> s{_ccatInputArtifactDetails = a});

-- | FIXME: Undocumented member.
ccatOutputArtifactDetails :: Lens' CreateCustomActionType ArtifactDetails
ccatOutputArtifactDetails = lens _ccatOutputArtifactDetails (\ s a -> s{_ccatOutputArtifactDetails = a});

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
              ["settings" .= _ccatSettings,
               "configurationProperties" .=
                 _ccatConfigurationProperties,
               "category" .= _ccatCategory,
               "provider" .= _ccatProvider,
               "version" .= _ccatVersion,
               "inputArtifactDetails" .= _ccatInputArtifactDetails,
               "outputArtifactDetails" .=
                 _ccatOutputArtifactDetails]

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
-- * 'ccatrStatus'
--
-- * 'ccatrActionType'
data CreateCustomActionTypeResponse = CreateCustomActionTypeResponse'
    { _ccatrStatus     :: !Int
    , _ccatrActionType :: !ActionType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateCustomActionTypeResponse' smart constructor.
createCustomActionTypeResponse :: Int -> ActionType -> CreateCustomActionTypeResponse
createCustomActionTypeResponse pStatus pActionType =
    CreateCustomActionTypeResponse'
    { _ccatrStatus = pStatus
    , _ccatrActionType = pActionType
    }

-- | FIXME: Undocumented member.
ccatrStatus :: Lens' CreateCustomActionTypeResponse Int
ccatrStatus = lens _ccatrStatus (\ s a -> s{_ccatrStatus = a});

-- | FIXME: Undocumented member.
ccatrActionType :: Lens' CreateCustomActionTypeResponse ActionType
ccatrActionType = lens _ccatrActionType (\ s a -> s{_ccatrActionType = a});

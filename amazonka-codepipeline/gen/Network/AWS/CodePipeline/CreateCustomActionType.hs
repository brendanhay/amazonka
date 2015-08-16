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
-- Module      : Network.AWS.CodePipeline.CreateCustomActionType
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom action that can be used in all pipelines associated
-- with the AWS account. Only used for custom actions.
--
-- /See:/ <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_CreateCustomActionType.html AWS API Reference> for CreateCustomActionType.
module Network.AWS.CodePipeline.CreateCustomActionType
    (
    -- * Creating a Request
      createCustomActionType
    , CreateCustomActionType
    -- * Request Lenses
    , ccatSettings
    , ccatConfigurationProperties
    , ccatCategory
    , ccatProvider
    , ccatVersion
    , ccatInputArtifactDetails
    , ccatOutputArtifactDetails

    -- * Destructuring the Response
    , createCustomActionTypeResponse
    , CreateCustomActionTypeResponse
    -- * Response Lenses
    , ccatrsStatus
    , ccatrsActionType
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.CodePipeline.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a create custom action operation.
--
-- /See:/ 'createCustomActionType' smart constructor.
data CreateCustomActionType = CreateCustomActionType'
    { _ccatSettings                :: !(Maybe ActionTypeSettings)
    , _ccatConfigurationProperties :: !(Maybe [ActionConfigurationProperty])
    , _ccatCategory                :: !ActionCategory
    , _ccatProvider                :: !Text
    , _ccatVersion                 :: !Text
    , _ccatInputArtifactDetails    :: !ArtifactDetails
    , _ccatOutputArtifactDetails   :: !ArtifactDetails
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateCustomActionType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
createCustomActionType
    :: ActionCategory -- ^ 'ccatCategory'
    -> Text -- ^ 'ccatProvider'
    -> Text -- ^ 'ccatVersion'
    -> ArtifactDetails -- ^ 'ccatInputArtifactDetails'
    -> ArtifactDetails -- ^ 'ccatOutputArtifactDetails'
    -> CreateCustomActionType
createCustomActionType pCategory_ pProvider_ pVersion_ pInputArtifactDetails_ pOutputArtifactDetails_ =
    CreateCustomActionType'
    { _ccatSettings = Nothing
    , _ccatConfigurationProperties = Nothing
    , _ccatCategory = pCategory_
    , _ccatProvider = pProvider_
    , _ccatVersion = pVersion_
    , _ccatInputArtifactDetails = pInputArtifactDetails_
    , _ccatOutputArtifactDetails = pOutputArtifactDetails_
    }

-- | Undocumented member.
ccatSettings :: Lens' CreateCustomActionType (Maybe ActionTypeSettings)
ccatSettings = lens _ccatSettings (\ s a -> s{_ccatSettings = a});

-- | The configuration properties for the custom action.
ccatConfigurationProperties :: Lens' CreateCustomActionType [ActionConfigurationProperty]
ccatConfigurationProperties = lens _ccatConfigurationProperties (\ s a -> s{_ccatConfigurationProperties = a}) . _Default . _Coerce;

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
-- '1'. This is required.
ccatVersion :: Lens' CreateCustomActionType Text
ccatVersion = lens _ccatVersion (\ s a -> s{_ccatVersion = a});

-- | Undocumented member.
ccatInputArtifactDetails :: Lens' CreateCustomActionType ArtifactDetails
ccatInputArtifactDetails = lens _ccatInputArtifactDetails (\ s a -> s{_ccatInputArtifactDetails = a});

-- | Undocumented member.
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
data CreateCustomActionTypeResponse = CreateCustomActionTypeResponse'
    { _ccatrsStatus     :: !Int
    , _ccatrsActionType :: !ActionType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateCustomActionTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccatrsStatus'
--
-- * 'ccatrsActionType'
createCustomActionTypeResponse
    :: Int -- ^ 'ccatrsStatus'
    -> ActionType -- ^ 'ccatrsActionType'
    -> CreateCustomActionTypeResponse
createCustomActionTypeResponse pStatus_ pActionType_ =
    CreateCustomActionTypeResponse'
    { _ccatrsStatus = pStatus_
    , _ccatrsActionType = pActionType_
    }

-- | The response status code.
ccatrsStatus :: Lens' CreateCustomActionTypeResponse Int
ccatrsStatus = lens _ccatrsStatus (\ s a -> s{_ccatrsStatus = a});

-- | Undocumented member.
ccatrsActionType :: Lens' CreateCustomActionTypeResponse ActionType
ccatrsActionType = lens _ccatrsActionType (\ s a -> s{_ccatrsActionType = a});

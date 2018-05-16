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
-- Module      : Network.AWS.ServerlessApplicationRepository.CreateApplicationVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application version.
--
--
module Network.AWS.ServerlessApplicationRepository.CreateApplicationVersion
    (
    -- * Creating a Request
      createApplicationVersion
    , CreateApplicationVersion
    -- * Request Lenses
    , cavSourceCodeURL
    , cavTemplateBody
    , cavTemplateURL
    , cavApplicationId
    , cavSemanticVersion

    -- * Destructuring the Response
    , createApplicationVersionResponse
    , CreateApplicationVersionResponse
    -- * Response Lenses
    , cavrsCreationTime
    , cavrsParameterDefinitions
    , cavrsSemanticVersion
    , cavrsSourceCodeURL
    , cavrsApplicationId
    , cavrsTemplateURL
    , cavrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServerlessApplicationRepository.Types
import Network.AWS.ServerlessApplicationRepository.Types.Product

-- | /See:/ 'createApplicationVersion' smart constructor.
data CreateApplicationVersion = CreateApplicationVersion'
  { _cavSourceCodeURL   :: !(Maybe Text)
  , _cavTemplateBody    :: !(Maybe Text)
  , _cavTemplateURL     :: !(Maybe Text)
  , _cavApplicationId   :: !Text
  , _cavSemanticVersion :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateApplicationVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cavSourceCodeURL' - A link to a public repository for the source code of your application.
--
-- * 'cavTemplateBody' - The raw packaged AWS SAM template of your application.
--
-- * 'cavTemplateURL' - A link to the packaged AWS SAM template of your application.
--
-- * 'cavApplicationId' - The ID of the application to get.
--
-- * 'cavSemanticVersion' - The semantic version of the new version.
createApplicationVersion
    :: Text -- ^ 'cavApplicationId'
    -> Text -- ^ 'cavSemanticVersion'
    -> CreateApplicationVersion
createApplicationVersion pApplicationId_ pSemanticVersion_ =
  CreateApplicationVersion'
    { _cavSourceCodeURL = Nothing
    , _cavTemplateBody = Nothing
    , _cavTemplateURL = Nothing
    , _cavApplicationId = pApplicationId_
    , _cavSemanticVersion = pSemanticVersion_
    }


-- | A link to a public repository for the source code of your application.
cavSourceCodeURL :: Lens' CreateApplicationVersion (Maybe Text)
cavSourceCodeURL = lens _cavSourceCodeURL (\ s a -> s{_cavSourceCodeURL = a})

-- | The raw packaged AWS SAM template of your application.
cavTemplateBody :: Lens' CreateApplicationVersion (Maybe Text)
cavTemplateBody = lens _cavTemplateBody (\ s a -> s{_cavTemplateBody = a})

-- | A link to the packaged AWS SAM template of your application.
cavTemplateURL :: Lens' CreateApplicationVersion (Maybe Text)
cavTemplateURL = lens _cavTemplateURL (\ s a -> s{_cavTemplateURL = a})

-- | The ID of the application to get.
cavApplicationId :: Lens' CreateApplicationVersion Text
cavApplicationId = lens _cavApplicationId (\ s a -> s{_cavApplicationId = a})

-- | The semantic version of the new version.
cavSemanticVersion :: Lens' CreateApplicationVersion Text
cavSemanticVersion = lens _cavSemanticVersion (\ s a -> s{_cavSemanticVersion = a})

instance AWSRequest CreateApplicationVersion where
        type Rs CreateApplicationVersion =
             CreateApplicationVersionResponse
        request = putJSON serverlessApplicationRepository
        response
          = receiveJSON
              (\ s h x ->
                 CreateApplicationVersionResponse' <$>
                   (x .?> "creationTime") <*>
                     (x .?> "parameterDefinitions" .!@ mempty)
                     <*> (x .?> "semanticVersion")
                     <*> (x .?> "sourceCodeUrl")
                     <*> (x .?> "applicationId")
                     <*> (x .?> "templateUrl")
                     <*> (pure (fromEnum s)))

instance Hashable CreateApplicationVersion where

instance NFData CreateApplicationVersion where

instance ToHeaders CreateApplicationVersion where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateApplicationVersion where
        toJSON CreateApplicationVersion'{..}
          = object
              (catMaybes
                 [("sourceCodeUrl" .=) <$> _cavSourceCodeURL,
                  ("templateBody" .=) <$> _cavTemplateBody,
                  ("templateUrl" .=) <$> _cavTemplateURL])

instance ToPath CreateApplicationVersion where
        toPath CreateApplicationVersion'{..}
          = mconcat
              ["/applications/", toBS _cavApplicationId,
               "/versions/", toBS _cavSemanticVersion]

instance ToQuery CreateApplicationVersion where
        toQuery = const mempty

-- | /See:/ 'createApplicationVersionResponse' smart constructor.
data CreateApplicationVersionResponse = CreateApplicationVersionResponse'
  { _cavrsCreationTime         :: !(Maybe Text)
  , _cavrsParameterDefinitions :: !(Maybe [ParameterDefinition])
  , _cavrsSemanticVersion      :: !(Maybe Text)
  , _cavrsSourceCodeURL        :: !(Maybe Text)
  , _cavrsApplicationId        :: !(Maybe Text)
  , _cavrsTemplateURL          :: !(Maybe Text)
  , _cavrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateApplicationVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cavrsCreationTime' - The date/time this resource was created.
--
-- * 'cavrsParameterDefinitions' - Array of parameter types supported by the application.
--
-- * 'cavrsSemanticVersion' - The semantic version of the application: <https://semver.org/ https://semver.org/>
--
-- * 'cavrsSourceCodeURL' - A link to a public repository for the source code of your application.
--
-- * 'cavrsApplicationId' - The application Amazon Resource Name (ARN).
--
-- * 'cavrsTemplateURL' - A link to the packaged AWS SAM template of your application.
--
-- * 'cavrsResponseStatus' - -- | The response status code.
createApplicationVersionResponse
    :: Int -- ^ 'cavrsResponseStatus'
    -> CreateApplicationVersionResponse
createApplicationVersionResponse pResponseStatus_ =
  CreateApplicationVersionResponse'
    { _cavrsCreationTime = Nothing
    , _cavrsParameterDefinitions = Nothing
    , _cavrsSemanticVersion = Nothing
    , _cavrsSourceCodeURL = Nothing
    , _cavrsApplicationId = Nothing
    , _cavrsTemplateURL = Nothing
    , _cavrsResponseStatus = pResponseStatus_
    }


-- | The date/time this resource was created.
cavrsCreationTime :: Lens' CreateApplicationVersionResponse (Maybe Text)
cavrsCreationTime = lens _cavrsCreationTime (\ s a -> s{_cavrsCreationTime = a})

-- | Array of parameter types supported by the application.
cavrsParameterDefinitions :: Lens' CreateApplicationVersionResponse [ParameterDefinition]
cavrsParameterDefinitions = lens _cavrsParameterDefinitions (\ s a -> s{_cavrsParameterDefinitions = a}) . _Default . _Coerce

-- | The semantic version of the application: <https://semver.org/ https://semver.org/>
cavrsSemanticVersion :: Lens' CreateApplicationVersionResponse (Maybe Text)
cavrsSemanticVersion = lens _cavrsSemanticVersion (\ s a -> s{_cavrsSemanticVersion = a})

-- | A link to a public repository for the source code of your application.
cavrsSourceCodeURL :: Lens' CreateApplicationVersionResponse (Maybe Text)
cavrsSourceCodeURL = lens _cavrsSourceCodeURL (\ s a -> s{_cavrsSourceCodeURL = a})

-- | The application Amazon Resource Name (ARN).
cavrsApplicationId :: Lens' CreateApplicationVersionResponse (Maybe Text)
cavrsApplicationId = lens _cavrsApplicationId (\ s a -> s{_cavrsApplicationId = a})

-- | A link to the packaged AWS SAM template of your application.
cavrsTemplateURL :: Lens' CreateApplicationVersionResponse (Maybe Text)
cavrsTemplateURL = lens _cavrsTemplateURL (\ s a -> s{_cavrsTemplateURL = a})

-- | -- | The response status code.
cavrsResponseStatus :: Lens' CreateApplicationVersionResponse Int
cavrsResponseStatus = lens _cavrsResponseStatus (\ s a -> s{_cavrsResponseStatus = a})

instance NFData CreateApplicationVersionResponse
         where

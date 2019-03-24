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
-- Module      : Network.AWS.ServerlessApplicationRepository.CreateCloudFormationTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS CloudFormation template.
--
--
module Network.AWS.ServerlessApplicationRepository.CreateCloudFormationTemplate
    (
    -- * Creating a Request
      createCloudFormationTemplate
    , CreateCloudFormationTemplate
    -- * Request Lenses
    , ccftSemanticVersion
    , ccftApplicationId

    -- * Destructuring the Response
    , createCloudFormationTemplateResponse
    , CreateCloudFormationTemplateResponse
    -- * Response Lenses
    , ccftrsCreationTime
    , ccftrsStatus
    , ccftrsTemplateId
    , ccftrsSemanticVersion
    , ccftrsApplicationId
    , ccftrsTemplateURL
    , ccftrsExpirationTime
    , ccftrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServerlessApplicationRepository.Types
import Network.AWS.ServerlessApplicationRepository.Types.Product

-- | /See:/ 'createCloudFormationTemplate' smart constructor.
data CreateCloudFormationTemplate = CreateCloudFormationTemplate'
  { _ccftSemanticVersion :: !(Maybe Text)
  , _ccftApplicationId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCloudFormationTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccftSemanticVersion' - The semantic version of the application: <https://semver.org/ https://semver.org/>
--
-- * 'ccftApplicationId' - The Amazon Resource Name (ARN) of the application.
createCloudFormationTemplate
    :: Text -- ^ 'ccftApplicationId'
    -> CreateCloudFormationTemplate
createCloudFormationTemplate pApplicationId_ =
  CreateCloudFormationTemplate'
    {_ccftSemanticVersion = Nothing, _ccftApplicationId = pApplicationId_}


-- | The semantic version of the application: <https://semver.org/ https://semver.org/>
ccftSemanticVersion :: Lens' CreateCloudFormationTemplate (Maybe Text)
ccftSemanticVersion = lens _ccftSemanticVersion (\ s a -> s{_ccftSemanticVersion = a})

-- | The Amazon Resource Name (ARN) of the application.
ccftApplicationId :: Lens' CreateCloudFormationTemplate Text
ccftApplicationId = lens _ccftApplicationId (\ s a -> s{_ccftApplicationId = a})

instance AWSRequest CreateCloudFormationTemplate
         where
        type Rs CreateCloudFormationTemplate =
             CreateCloudFormationTemplateResponse
        request = postJSON serverlessApplicationRepository
        response
          = receiveJSON
              (\ s h x ->
                 CreateCloudFormationTemplateResponse' <$>
                   (x .?> "creationTime") <*> (x .?> "status") <*>
                     (x .?> "templateId")
                     <*> (x .?> "semanticVersion")
                     <*> (x .?> "applicationId")
                     <*> (x .?> "templateUrl")
                     <*> (x .?> "expirationTime")
                     <*> (pure (fromEnum s)))

instance Hashable CreateCloudFormationTemplate where

instance NFData CreateCloudFormationTemplate where

instance ToHeaders CreateCloudFormationTemplate where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateCloudFormationTemplate where
        toJSON CreateCloudFormationTemplate'{..}
          = object
              (catMaybes
                 [("semanticVersion" .=) <$> _ccftSemanticVersion])

instance ToPath CreateCloudFormationTemplate where
        toPath CreateCloudFormationTemplate'{..}
          = mconcat
              ["/applications/", toBS _ccftApplicationId,
               "/templates"]

instance ToQuery CreateCloudFormationTemplate where
        toQuery = const mempty

-- | /See:/ 'createCloudFormationTemplateResponse' smart constructor.
data CreateCloudFormationTemplateResponse = CreateCloudFormationTemplateResponse'
  { _ccftrsCreationTime    :: !(Maybe Text)
  , _ccftrsStatus          :: !(Maybe Status)
  , _ccftrsTemplateId      :: !(Maybe Text)
  , _ccftrsSemanticVersion :: !(Maybe Text)
  , _ccftrsApplicationId   :: !(Maybe Text)
  , _ccftrsTemplateURL     :: !(Maybe Text)
  , _ccftrsExpirationTime  :: !(Maybe Text)
  , _ccftrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCloudFormationTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccftrsCreationTime' - The date and time this resource was created.
--
-- * 'ccftrsStatus' - Status of the template creation workflow. Possible values: PREPARING | ACTIVE | EXPIRED
--
-- * 'ccftrsTemplateId' - The UUID returned by CreateCloudFormationTemplate. Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
--
-- * 'ccftrsSemanticVersion' - The semantic version of the application: <https://semver.org/ https://semver.org/>
--
-- * 'ccftrsApplicationId' - The application Amazon Resource Name (ARN).
--
-- * 'ccftrsTemplateURL' - A link to the template that can be used to deploy the application using  AWS CloudFormation.
--
-- * 'ccftrsExpirationTime' - The date and time this template expires. Templates  expire 1 hour after creation.
--
-- * 'ccftrsResponseStatus' - -- | The response status code.
createCloudFormationTemplateResponse
    :: Int -- ^ 'ccftrsResponseStatus'
    -> CreateCloudFormationTemplateResponse
createCloudFormationTemplateResponse pResponseStatus_ =
  CreateCloudFormationTemplateResponse'
    { _ccftrsCreationTime = Nothing
    , _ccftrsStatus = Nothing
    , _ccftrsTemplateId = Nothing
    , _ccftrsSemanticVersion = Nothing
    , _ccftrsApplicationId = Nothing
    , _ccftrsTemplateURL = Nothing
    , _ccftrsExpirationTime = Nothing
    , _ccftrsResponseStatus = pResponseStatus_
    }


-- | The date and time this resource was created.
ccftrsCreationTime :: Lens' CreateCloudFormationTemplateResponse (Maybe Text)
ccftrsCreationTime = lens _ccftrsCreationTime (\ s a -> s{_ccftrsCreationTime = a})

-- | Status of the template creation workflow. Possible values: PREPARING | ACTIVE | EXPIRED
ccftrsStatus :: Lens' CreateCloudFormationTemplateResponse (Maybe Status)
ccftrsStatus = lens _ccftrsStatus (\ s a -> s{_ccftrsStatus = a})

-- | The UUID returned by CreateCloudFormationTemplate. Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
ccftrsTemplateId :: Lens' CreateCloudFormationTemplateResponse (Maybe Text)
ccftrsTemplateId = lens _ccftrsTemplateId (\ s a -> s{_ccftrsTemplateId = a})

-- | The semantic version of the application: <https://semver.org/ https://semver.org/>
ccftrsSemanticVersion :: Lens' CreateCloudFormationTemplateResponse (Maybe Text)
ccftrsSemanticVersion = lens _ccftrsSemanticVersion (\ s a -> s{_ccftrsSemanticVersion = a})

-- | The application Amazon Resource Name (ARN).
ccftrsApplicationId :: Lens' CreateCloudFormationTemplateResponse (Maybe Text)
ccftrsApplicationId = lens _ccftrsApplicationId (\ s a -> s{_ccftrsApplicationId = a})

-- | A link to the template that can be used to deploy the application using  AWS CloudFormation.
ccftrsTemplateURL :: Lens' CreateCloudFormationTemplateResponse (Maybe Text)
ccftrsTemplateURL = lens _ccftrsTemplateURL (\ s a -> s{_ccftrsTemplateURL = a})

-- | The date and time this template expires. Templates  expire 1 hour after creation.
ccftrsExpirationTime :: Lens' CreateCloudFormationTemplateResponse (Maybe Text)
ccftrsExpirationTime = lens _ccftrsExpirationTime (\ s a -> s{_ccftrsExpirationTime = a})

-- | -- | The response status code.
ccftrsResponseStatus :: Lens' CreateCloudFormationTemplateResponse Int
ccftrsResponseStatus = lens _ccftrsResponseStatus (\ s a -> s{_ccftrsResponseStatus = a})

instance NFData CreateCloudFormationTemplateResponse
         where

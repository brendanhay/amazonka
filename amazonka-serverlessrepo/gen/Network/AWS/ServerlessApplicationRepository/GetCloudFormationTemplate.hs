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
-- Module      : Network.AWS.ServerlessApplicationRepository.GetCloudFormationTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified AWS CloudFormation template.
--
--
module Network.AWS.ServerlessApplicationRepository.GetCloudFormationTemplate
    (
    -- * Creating a Request
      getCloudFormationTemplate
    , GetCloudFormationTemplate
    -- * Request Lenses
    , gcftApplicationId
    , gcftTemplateId

    -- * Destructuring the Response
    , getCloudFormationTemplateResponse
    , GetCloudFormationTemplateResponse
    -- * Response Lenses
    , gcftrsCreationTime
    , gcftrsStatus
    , gcftrsTemplateId
    , gcftrsSemanticVersion
    , gcftrsApplicationId
    , gcftrsTemplateURL
    , gcftrsExpirationTime
    , gcftrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServerlessApplicationRepository.Types
import Network.AWS.ServerlessApplicationRepository.Types.Product

-- | /See:/ 'getCloudFormationTemplate' smart constructor.
data GetCloudFormationTemplate = GetCloudFormationTemplate'
  { _gcftApplicationId :: !Text
  , _gcftTemplateId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCloudFormationTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcftApplicationId' - The Amazon Resource Name (ARN) of the application.
--
-- * 'gcftTemplateId' - The UUID returned by CreateCloudFormationTemplate. Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
getCloudFormationTemplate
    :: Text -- ^ 'gcftApplicationId'
    -> Text -- ^ 'gcftTemplateId'
    -> GetCloudFormationTemplate
getCloudFormationTemplate pApplicationId_ pTemplateId_ =
  GetCloudFormationTemplate'
    {_gcftApplicationId = pApplicationId_, _gcftTemplateId = pTemplateId_}


-- | The Amazon Resource Name (ARN) of the application.
gcftApplicationId :: Lens' GetCloudFormationTemplate Text
gcftApplicationId = lens _gcftApplicationId (\ s a -> s{_gcftApplicationId = a})

-- | The UUID returned by CreateCloudFormationTemplate. Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
gcftTemplateId :: Lens' GetCloudFormationTemplate Text
gcftTemplateId = lens _gcftTemplateId (\ s a -> s{_gcftTemplateId = a})

instance AWSRequest GetCloudFormationTemplate where
        type Rs GetCloudFormationTemplate =
             GetCloudFormationTemplateResponse
        request = get serverlessApplicationRepository
        response
          = receiveJSON
              (\ s h x ->
                 GetCloudFormationTemplateResponse' <$>
                   (x .?> "creationTime") <*> (x .?> "status") <*>
                     (x .?> "templateId")
                     <*> (x .?> "semanticVersion")
                     <*> (x .?> "applicationId")
                     <*> (x .?> "templateUrl")
                     <*> (x .?> "expirationTime")
                     <*> (pure (fromEnum s)))

instance Hashable GetCloudFormationTemplate where

instance NFData GetCloudFormationTemplate where

instance ToHeaders GetCloudFormationTemplate where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetCloudFormationTemplate where
        toPath GetCloudFormationTemplate'{..}
          = mconcat
              ["/applications/", toBS _gcftApplicationId,
               "/templates/", toBS _gcftTemplateId]

instance ToQuery GetCloudFormationTemplate where
        toQuery = const mempty

-- | /See:/ 'getCloudFormationTemplateResponse' smart constructor.
data GetCloudFormationTemplateResponse = GetCloudFormationTemplateResponse'
  { _gcftrsCreationTime    :: !(Maybe Text)
  , _gcftrsStatus          :: !(Maybe Status)
  , _gcftrsTemplateId      :: !(Maybe Text)
  , _gcftrsSemanticVersion :: !(Maybe Text)
  , _gcftrsApplicationId   :: !(Maybe Text)
  , _gcftrsTemplateURL     :: !(Maybe Text)
  , _gcftrsExpirationTime  :: !(Maybe Text)
  , _gcftrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCloudFormationTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcftrsCreationTime' - The date and time this resource was created.
--
-- * 'gcftrsStatus' - Status of the template creation workflow. Possible values: PREPARING | ACTIVE | EXPIRED
--
-- * 'gcftrsTemplateId' - The UUID returned by CreateCloudFormationTemplate. Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
--
-- * 'gcftrsSemanticVersion' - The semantic version of the application: <https://semver.org/ https://semver.org/>
--
-- * 'gcftrsApplicationId' - The application Amazon Resource Name (ARN).
--
-- * 'gcftrsTemplateURL' - A link to the template that can be used to deploy the application using  AWS CloudFormation.
--
-- * 'gcftrsExpirationTime' - The date and time this template expires. Templates  expire 1 hour after creation.
--
-- * 'gcftrsResponseStatus' - -- | The response status code.
getCloudFormationTemplateResponse
    :: Int -- ^ 'gcftrsResponseStatus'
    -> GetCloudFormationTemplateResponse
getCloudFormationTemplateResponse pResponseStatus_ =
  GetCloudFormationTemplateResponse'
    { _gcftrsCreationTime = Nothing
    , _gcftrsStatus = Nothing
    , _gcftrsTemplateId = Nothing
    , _gcftrsSemanticVersion = Nothing
    , _gcftrsApplicationId = Nothing
    , _gcftrsTemplateURL = Nothing
    , _gcftrsExpirationTime = Nothing
    , _gcftrsResponseStatus = pResponseStatus_
    }


-- | The date and time this resource was created.
gcftrsCreationTime :: Lens' GetCloudFormationTemplateResponse (Maybe Text)
gcftrsCreationTime = lens _gcftrsCreationTime (\ s a -> s{_gcftrsCreationTime = a})

-- | Status of the template creation workflow. Possible values: PREPARING | ACTIVE | EXPIRED
gcftrsStatus :: Lens' GetCloudFormationTemplateResponse (Maybe Status)
gcftrsStatus = lens _gcftrsStatus (\ s a -> s{_gcftrsStatus = a})

-- | The UUID returned by CreateCloudFormationTemplate. Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
gcftrsTemplateId :: Lens' GetCloudFormationTemplateResponse (Maybe Text)
gcftrsTemplateId = lens _gcftrsTemplateId (\ s a -> s{_gcftrsTemplateId = a})

-- | The semantic version of the application: <https://semver.org/ https://semver.org/>
gcftrsSemanticVersion :: Lens' GetCloudFormationTemplateResponse (Maybe Text)
gcftrsSemanticVersion = lens _gcftrsSemanticVersion (\ s a -> s{_gcftrsSemanticVersion = a})

-- | The application Amazon Resource Name (ARN).
gcftrsApplicationId :: Lens' GetCloudFormationTemplateResponse (Maybe Text)
gcftrsApplicationId = lens _gcftrsApplicationId (\ s a -> s{_gcftrsApplicationId = a})

-- | A link to the template that can be used to deploy the application using  AWS CloudFormation.
gcftrsTemplateURL :: Lens' GetCloudFormationTemplateResponse (Maybe Text)
gcftrsTemplateURL = lens _gcftrsTemplateURL (\ s a -> s{_gcftrsTemplateURL = a})

-- | The date and time this template expires. Templates  expire 1 hour after creation.
gcftrsExpirationTime :: Lens' GetCloudFormationTemplateResponse (Maybe Text)
gcftrsExpirationTime = lens _gcftrsExpirationTime (\ s a -> s{_gcftrsExpirationTime = a})

-- | -- | The response status code.
gcftrsResponseStatus :: Lens' GetCloudFormationTemplateResponse Int
gcftrsResponseStatus = lens _gcftrsResponseStatus (\ s a -> s{_gcftrsResponseStatus = a})

instance NFData GetCloudFormationTemplateResponse
         where

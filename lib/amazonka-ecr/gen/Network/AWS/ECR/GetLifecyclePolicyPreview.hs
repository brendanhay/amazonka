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
-- Module      : Network.AWS.ECR.GetLifecyclePolicyPreview
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the results of the specified lifecycle policy preview request.
--
--
module Network.AWS.ECR.GetLifecyclePolicyPreview
    (
    -- * Creating a Request
      getLifecyclePolicyPreview
    , GetLifecyclePolicyPreview
    -- * Request Lenses
    , glppRegistryId
    , glppImageIds
    , glppNextToken
    , glppFilter
    , glppMaxResults
    , glppRepositoryName

    -- * Destructuring the Response
    , getLifecyclePolicyPreviewResponse
    , GetLifecyclePolicyPreviewResponse
    -- * Response Lenses
    , glpprsSummary
    , glpprsStatus
    , glpprsRegistryId
    , glpprsLifecyclePolicyText
    , glpprsNextToken
    , glpprsRepositoryName
    , glpprsPreviewResults
    , glpprsResponseStatus
    ) where

import Network.AWS.ECR.Types
import Network.AWS.ECR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getLifecyclePolicyPreview' smart constructor.
data GetLifecyclePolicyPreview = GetLifecyclePolicyPreview'
  { _glppRegistryId     :: !(Maybe Text)
  , _glppImageIds       :: !(Maybe [ImageIdentifier])
  , _glppNextToken      :: !(Maybe Text)
  , _glppFilter         :: !(Maybe LifecyclePolicyPreviewFilter)
  , _glppMaxResults     :: !(Maybe Nat)
  , _glppRepositoryName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLifecyclePolicyPreview' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glppRegistryId' - The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- * 'glppImageIds' - The list of imageIDs to be included.
--
-- * 'glppNextToken' - The @nextToken@ value returned from a previous paginated  @GetLifecyclePolicyPreviewRequest@ request where @maxResults@ was used and the  results exceeded the value of that parameter. Pagination continues from the end of the  previous results that returned the @nextToken@ value. This value is  @null@ when there are no more results to return. This option cannot be used when you specify images with @imageIds@ .
--
-- * 'glppFilter' - An optional parameter that filters results based on image tag status and all tags, if tagged.
--
-- * 'glppMaxResults' - The maximum number of repository results returned by @GetLifecyclePolicyPreviewRequest@ in  paginated output. When this parameter is used, @GetLifecyclePolicyPreviewRequest@ only returns  @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending  another @GetLifecyclePolicyPreviewRequest@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this  parameter is not used, then @GetLifecyclePolicyPreviewRequest@ returns up to  100 results and a @nextToken@ value, if  applicable. This option cannot be used when you specify images with @imageIds@ .
--
-- * 'glppRepositoryName' - The name of the repository.
getLifecyclePolicyPreview
    :: Text -- ^ 'glppRepositoryName'
    -> GetLifecyclePolicyPreview
getLifecyclePolicyPreview pRepositoryName_ =
  GetLifecyclePolicyPreview'
    { _glppRegistryId = Nothing
    , _glppImageIds = Nothing
    , _glppNextToken = Nothing
    , _glppFilter = Nothing
    , _glppMaxResults = Nothing
    , _glppRepositoryName = pRepositoryName_
    }


-- | The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
glppRegistryId :: Lens' GetLifecyclePolicyPreview (Maybe Text)
glppRegistryId = lens _glppRegistryId (\ s a -> s{_glppRegistryId = a})

-- | The list of imageIDs to be included.
glppImageIds :: Lens' GetLifecyclePolicyPreview [ImageIdentifier]
glppImageIds = lens _glppImageIds (\ s a -> s{_glppImageIds = a}) . _Default . _Coerce

-- | The @nextToken@ value returned from a previous paginated  @GetLifecyclePolicyPreviewRequest@ request where @maxResults@ was used and the  results exceeded the value of that parameter. Pagination continues from the end of the  previous results that returned the @nextToken@ value. This value is  @null@ when there are no more results to return. This option cannot be used when you specify images with @imageIds@ .
glppNextToken :: Lens' GetLifecyclePolicyPreview (Maybe Text)
glppNextToken = lens _glppNextToken (\ s a -> s{_glppNextToken = a})

-- | An optional parameter that filters results based on image tag status and all tags, if tagged.
glppFilter :: Lens' GetLifecyclePolicyPreview (Maybe LifecyclePolicyPreviewFilter)
glppFilter = lens _glppFilter (\ s a -> s{_glppFilter = a})

-- | The maximum number of repository results returned by @GetLifecyclePolicyPreviewRequest@ in  paginated output. When this parameter is used, @GetLifecyclePolicyPreviewRequest@ only returns  @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending  another @GetLifecyclePolicyPreviewRequest@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this  parameter is not used, then @GetLifecyclePolicyPreviewRequest@ returns up to  100 results and a @nextToken@ value, if  applicable. This option cannot be used when you specify images with @imageIds@ .
glppMaxResults :: Lens' GetLifecyclePolicyPreview (Maybe Natural)
glppMaxResults = lens _glppMaxResults (\ s a -> s{_glppMaxResults = a}) . mapping _Nat

-- | The name of the repository.
glppRepositoryName :: Lens' GetLifecyclePolicyPreview Text
glppRepositoryName = lens _glppRepositoryName (\ s a -> s{_glppRepositoryName = a})

instance AWSRequest GetLifecyclePolicyPreview where
        type Rs GetLifecyclePolicyPreview =
             GetLifecyclePolicyPreviewResponse
        request = postJSON ecr
        response
          = receiveJSON
              (\ s h x ->
                 GetLifecyclePolicyPreviewResponse' <$>
                   (x .?> "summary") <*> (x .?> "status") <*>
                     (x .?> "registryId")
                     <*> (x .?> "lifecyclePolicyText")
                     <*> (x .?> "nextToken")
                     <*> (x .?> "repositoryName")
                     <*> (x .?> "previewResults" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetLifecyclePolicyPreview where

instance NFData GetLifecyclePolicyPreview where

instance ToHeaders GetLifecyclePolicyPreview where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerRegistry_V20150921.GetLifecyclePolicyPreview"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetLifecyclePolicyPreview where
        toJSON GetLifecyclePolicyPreview'{..}
          = object
              (catMaybes
                 [("registryId" .=) <$> _glppRegistryId,
                  ("imageIds" .=) <$> _glppImageIds,
                  ("nextToken" .=) <$> _glppNextToken,
                  ("filter" .=) <$> _glppFilter,
                  ("maxResults" .=) <$> _glppMaxResults,
                  Just ("repositoryName" .= _glppRepositoryName)])

instance ToPath GetLifecyclePolicyPreview where
        toPath = const "/"

instance ToQuery GetLifecyclePolicyPreview where
        toQuery = const mempty

-- | /See:/ 'getLifecyclePolicyPreviewResponse' smart constructor.
data GetLifecyclePolicyPreviewResponse = GetLifecyclePolicyPreviewResponse'
  { _glpprsSummary             :: !(Maybe LifecyclePolicyPreviewSummary)
  , _glpprsStatus              :: !(Maybe LifecyclePolicyPreviewStatus)
  , _glpprsRegistryId          :: !(Maybe Text)
  , _glpprsLifecyclePolicyText :: !(Maybe Text)
  , _glpprsNextToken           :: !(Maybe Text)
  , _glpprsRepositoryName      :: !(Maybe Text)
  , _glpprsPreviewResults      :: !(Maybe [LifecyclePolicyPreviewResult])
  , _glpprsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLifecyclePolicyPreviewResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glpprsSummary' - The list of images that is returned as a result of the action.
--
-- * 'glpprsStatus' - The status of the lifecycle policy preview request.
--
-- * 'glpprsRegistryId' - The registry ID associated with the request.
--
-- * 'glpprsLifecyclePolicyText' - The JSON lifecycle policy text.
--
-- * 'glpprsNextToken' - The @nextToken@ value to include in a future @GetLifecyclePolicyPreview@ request. When the results of a @GetLifecyclePolicyPreview@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'glpprsRepositoryName' - The repository name associated with the request.
--
-- * 'glpprsPreviewResults' - The results of the lifecycle policy preview request.
--
-- * 'glpprsResponseStatus' - -- | The response status code.
getLifecyclePolicyPreviewResponse
    :: Int -- ^ 'glpprsResponseStatus'
    -> GetLifecyclePolicyPreviewResponse
getLifecyclePolicyPreviewResponse pResponseStatus_ =
  GetLifecyclePolicyPreviewResponse'
    { _glpprsSummary = Nothing
    , _glpprsStatus = Nothing
    , _glpprsRegistryId = Nothing
    , _glpprsLifecyclePolicyText = Nothing
    , _glpprsNextToken = Nothing
    , _glpprsRepositoryName = Nothing
    , _glpprsPreviewResults = Nothing
    , _glpprsResponseStatus = pResponseStatus_
    }


-- | The list of images that is returned as a result of the action.
glpprsSummary :: Lens' GetLifecyclePolicyPreviewResponse (Maybe LifecyclePolicyPreviewSummary)
glpprsSummary = lens _glpprsSummary (\ s a -> s{_glpprsSummary = a})

-- | The status of the lifecycle policy preview request.
glpprsStatus :: Lens' GetLifecyclePolicyPreviewResponse (Maybe LifecyclePolicyPreviewStatus)
glpprsStatus = lens _glpprsStatus (\ s a -> s{_glpprsStatus = a})

-- | The registry ID associated with the request.
glpprsRegistryId :: Lens' GetLifecyclePolicyPreviewResponse (Maybe Text)
glpprsRegistryId = lens _glpprsRegistryId (\ s a -> s{_glpprsRegistryId = a})

-- | The JSON lifecycle policy text.
glpprsLifecyclePolicyText :: Lens' GetLifecyclePolicyPreviewResponse (Maybe Text)
glpprsLifecyclePolicyText = lens _glpprsLifecyclePolicyText (\ s a -> s{_glpprsLifecyclePolicyText = a})

-- | The @nextToken@ value to include in a future @GetLifecyclePolicyPreview@ request. When the results of a @GetLifecyclePolicyPreview@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
glpprsNextToken :: Lens' GetLifecyclePolicyPreviewResponse (Maybe Text)
glpprsNextToken = lens _glpprsNextToken (\ s a -> s{_glpprsNextToken = a})

-- | The repository name associated with the request.
glpprsRepositoryName :: Lens' GetLifecyclePolicyPreviewResponse (Maybe Text)
glpprsRepositoryName = lens _glpprsRepositoryName (\ s a -> s{_glpprsRepositoryName = a})

-- | The results of the lifecycle policy preview request.
glpprsPreviewResults :: Lens' GetLifecyclePolicyPreviewResponse [LifecyclePolicyPreviewResult]
glpprsPreviewResults = lens _glpprsPreviewResults (\ s a -> s{_glpprsPreviewResults = a}) . _Default . _Coerce

-- | -- | The response status code.
glpprsResponseStatus :: Lens' GetLifecyclePolicyPreviewResponse Int
glpprsResponseStatus = lens _glpprsResponseStatus (\ s a -> s{_glpprsResponseStatus = a})

instance NFData GetLifecyclePolicyPreviewResponse
         where

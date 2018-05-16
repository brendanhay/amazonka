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
-- Module      : Network.AWS.Greengrass.GetDeploymentStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the status of a deployment.
module Network.AWS.Greengrass.GetDeploymentStatus
    (
    -- * Creating a Request
      getDeploymentStatus
    , GetDeploymentStatus
    -- * Request Lenses
    , gdsGroupId
    , gdsDeploymentId

    -- * Destructuring the Response
    , getDeploymentStatusResponse
    , GetDeploymentStatusResponse
    -- * Response Lenses
    , gdsrsDeploymentType
    , gdsrsErrorDetails
    , gdsrsDeploymentStatus
    , gdsrsUpdatedAt
    , gdsrsErrorMessage
    , gdsrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDeploymentStatus' smart constructor.
data GetDeploymentStatus = GetDeploymentStatus'
  { _gdsGroupId      :: !Text
  , _gdsDeploymentId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDeploymentStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdsGroupId' - The ID of the AWS Greengrass group.
--
-- * 'gdsDeploymentId' - The ID of the deployment.
getDeploymentStatus
    :: Text -- ^ 'gdsGroupId'
    -> Text -- ^ 'gdsDeploymentId'
    -> GetDeploymentStatus
getDeploymentStatus pGroupId_ pDeploymentId_ =
  GetDeploymentStatus'
    {_gdsGroupId = pGroupId_, _gdsDeploymentId = pDeploymentId_}


-- | The ID of the AWS Greengrass group.
gdsGroupId :: Lens' GetDeploymentStatus Text
gdsGroupId = lens _gdsGroupId (\ s a -> s{_gdsGroupId = a})

-- | The ID of the deployment.
gdsDeploymentId :: Lens' GetDeploymentStatus Text
gdsDeploymentId = lens _gdsDeploymentId (\ s a -> s{_gdsDeploymentId = a})

instance AWSRequest GetDeploymentStatus where
        type Rs GetDeploymentStatus =
             GetDeploymentStatusResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 GetDeploymentStatusResponse' <$>
                   (x .?> "DeploymentType") <*>
                     (x .?> "ErrorDetails" .!@ mempty)
                     <*> (x .?> "DeploymentStatus")
                     <*> (x .?> "UpdatedAt")
                     <*> (x .?> "ErrorMessage")
                     <*> (pure (fromEnum s)))

instance Hashable GetDeploymentStatus where

instance NFData GetDeploymentStatus where

instance ToHeaders GetDeploymentStatus where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetDeploymentStatus where
        toPath GetDeploymentStatus'{..}
          = mconcat
              ["/greengrass/groups/", toBS _gdsGroupId,
               "/deployments/", toBS _gdsDeploymentId, "/status"]

instance ToQuery GetDeploymentStatus where
        toQuery = const mempty

-- | /See:/ 'getDeploymentStatusResponse' smart constructor.
data GetDeploymentStatusResponse = GetDeploymentStatusResponse'
  { _gdsrsDeploymentType   :: !(Maybe DeploymentType)
  , _gdsrsErrorDetails     :: !(Maybe [ErrorDetail])
  , _gdsrsDeploymentStatus :: !(Maybe Text)
  , _gdsrsUpdatedAt        :: !(Maybe Text)
  , _gdsrsErrorMessage     :: !(Maybe Text)
  , _gdsrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDeploymentStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdsrsDeploymentType' - The type of the deployment.
--
-- * 'gdsrsErrorDetails' - Error details
--
-- * 'gdsrsDeploymentStatus' - The status of the deployment.
--
-- * 'gdsrsUpdatedAt' - The time, in milliseconds since the epoch, when the deployment status was updated.
--
-- * 'gdsrsErrorMessage' - Error message
--
-- * 'gdsrsResponseStatus' - -- | The response status code.
getDeploymentStatusResponse
    :: Int -- ^ 'gdsrsResponseStatus'
    -> GetDeploymentStatusResponse
getDeploymentStatusResponse pResponseStatus_ =
  GetDeploymentStatusResponse'
    { _gdsrsDeploymentType = Nothing
    , _gdsrsErrorDetails = Nothing
    , _gdsrsDeploymentStatus = Nothing
    , _gdsrsUpdatedAt = Nothing
    , _gdsrsErrorMessage = Nothing
    , _gdsrsResponseStatus = pResponseStatus_
    }


-- | The type of the deployment.
gdsrsDeploymentType :: Lens' GetDeploymentStatusResponse (Maybe DeploymentType)
gdsrsDeploymentType = lens _gdsrsDeploymentType (\ s a -> s{_gdsrsDeploymentType = a})

-- | Error details
gdsrsErrorDetails :: Lens' GetDeploymentStatusResponse [ErrorDetail]
gdsrsErrorDetails = lens _gdsrsErrorDetails (\ s a -> s{_gdsrsErrorDetails = a}) . _Default . _Coerce

-- | The status of the deployment.
gdsrsDeploymentStatus :: Lens' GetDeploymentStatusResponse (Maybe Text)
gdsrsDeploymentStatus = lens _gdsrsDeploymentStatus (\ s a -> s{_gdsrsDeploymentStatus = a})

-- | The time, in milliseconds since the epoch, when the deployment status was updated.
gdsrsUpdatedAt :: Lens' GetDeploymentStatusResponse (Maybe Text)
gdsrsUpdatedAt = lens _gdsrsUpdatedAt (\ s a -> s{_gdsrsUpdatedAt = a})

-- | Error message
gdsrsErrorMessage :: Lens' GetDeploymentStatusResponse (Maybe Text)
gdsrsErrorMessage = lens _gdsrsErrorMessage (\ s a -> s{_gdsrsErrorMessage = a})

-- | -- | The response status code.
gdsrsResponseStatus :: Lens' GetDeploymentStatusResponse Int
gdsrsResponseStatus = lens _gdsrsResponseStatus (\ s a -> s{_gdsrsResponseStatus = a})

instance NFData GetDeploymentStatusResponse where

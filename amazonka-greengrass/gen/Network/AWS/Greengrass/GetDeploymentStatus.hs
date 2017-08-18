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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
    , gdsrsDeploymentStatus
    , gdsrsUpdatedAt
    , gdsrsErrorMessage
    , gdsrsResponseStatus
    ) where

import           Network.AWS.Greengrass.Types
import           Network.AWS.Greengrass.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getDeploymentStatus' smart constructor.
data GetDeploymentStatus = GetDeploymentStatus'
    { _gdsGroupId      :: !Text
    , _gdsDeploymentId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetDeploymentStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdsGroupId' - The unique Id of the AWS Greengrass Group
--
-- * 'gdsDeploymentId' - the deployment Id
getDeploymentStatus
    :: Text -- ^ 'gdsGroupId'
    -> Text -- ^ 'gdsDeploymentId'
    -> GetDeploymentStatus
getDeploymentStatus pGroupId_ pDeploymentId_ =
    GetDeploymentStatus'
    { _gdsGroupId = pGroupId_
    , _gdsDeploymentId = pDeploymentId_
    }

-- | The unique Id of the AWS Greengrass Group
gdsGroupId :: Lens' GetDeploymentStatus Text
gdsGroupId = lens _gdsGroupId (\ s a -> s{_gdsGroupId = a});

-- | the deployment Id
gdsDeploymentId :: Lens' GetDeploymentStatus Text
gdsDeploymentId = lens _gdsDeploymentId (\ s a -> s{_gdsDeploymentId = a});

instance AWSRequest GetDeploymentStatus where
        type Rs GetDeploymentStatus =
             GetDeploymentStatusResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 GetDeploymentStatusResponse' <$>
                   (x .?> "DeploymentStatus") <*> (x .?> "UpdatedAt")
                     <*> (x .?> "ErrorMessage")
                     <*> (pure (fromEnum s)))

instance Hashable GetDeploymentStatus

instance NFData GetDeploymentStatus

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
    { _gdsrsDeploymentStatus :: !(Maybe Text)
    , _gdsrsUpdatedAt        :: !(Maybe Text)
    , _gdsrsErrorMessage     :: !(Maybe Text)
    , _gdsrsResponseStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetDeploymentStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdsrsDeploymentStatus' - Status of the deployment.
--
-- * 'gdsrsUpdatedAt' - Last time the deployment status was updated.
--
-- * 'gdsrsErrorMessage' - Error Message
--
-- * 'gdsrsResponseStatus' - -- | The response status code.
getDeploymentStatusResponse
    :: Int -- ^ 'gdsrsResponseStatus'
    -> GetDeploymentStatusResponse
getDeploymentStatusResponse pResponseStatus_ =
    GetDeploymentStatusResponse'
    { _gdsrsDeploymentStatus = Nothing
    , _gdsrsUpdatedAt = Nothing
    , _gdsrsErrorMessage = Nothing
    , _gdsrsResponseStatus = pResponseStatus_
    }

-- | Status of the deployment.
gdsrsDeploymentStatus :: Lens' GetDeploymentStatusResponse (Maybe Text)
gdsrsDeploymentStatus = lens _gdsrsDeploymentStatus (\ s a -> s{_gdsrsDeploymentStatus = a});

-- | Last time the deployment status was updated.
gdsrsUpdatedAt :: Lens' GetDeploymentStatusResponse (Maybe Text)
gdsrsUpdatedAt = lens _gdsrsUpdatedAt (\ s a -> s{_gdsrsUpdatedAt = a});

-- | Error Message
gdsrsErrorMessage :: Lens' GetDeploymentStatusResponse (Maybe Text)
gdsrsErrorMessage = lens _gdsrsErrorMessage (\ s a -> s{_gdsrsErrorMessage = a});

-- | -- | The response status code.
gdsrsResponseStatus :: Lens' GetDeploymentStatusResponse Int
gdsrsResponseStatus = lens _gdsrsResponseStatus (\ s a -> s{_gdsrsResponseStatus = a});

instance NFData GetDeploymentStatusResponse

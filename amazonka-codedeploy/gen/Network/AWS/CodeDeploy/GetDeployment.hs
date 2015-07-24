{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.GetDeployment
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a deployment.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_GetDeployment.html>
module Network.AWS.CodeDeploy.GetDeployment
    (
    -- * Request
      GetDeployment
    -- ** Request constructor
    , getDeployment
    -- ** Request lenses
    , gdDeploymentId

    -- * Response
    , GetDeploymentResponse
    -- ** Response constructor
    , getDeploymentResponse
    -- ** Response lenses
    , gdrsDeploymentInfo
    , gdrsStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a get deployment operation.
--
-- /See:/ 'getDeployment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdDeploymentId'
newtype GetDeployment = GetDeployment'
    { _gdDeploymentId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDeployment' smart constructor.
getDeployment :: Text -> GetDeployment
getDeployment pDeploymentId_ =
    GetDeployment'
    { _gdDeploymentId = pDeploymentId_
    }

-- | An existing deployment ID associated with the applicable IAM user or AWS
-- account.
gdDeploymentId :: Lens' GetDeployment Text
gdDeploymentId = lens _gdDeploymentId (\ s a -> s{_gdDeploymentId = a});

instance AWSRequest GetDeployment where
        type Sv GetDeployment = CodeDeploy
        type Rs GetDeployment = GetDeploymentResponse
        request = postJSON "GetDeployment"
        response
          = receiveJSON
              (\ s h x ->
                 GetDeploymentResponse' <$>
                   (x .?> "deploymentInfo") <*> (pure (fromEnum s)))

instance ToHeaders GetDeployment where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.GetDeployment" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDeployment where
        toJSON GetDeployment'{..}
          = object ["deploymentId" .= _gdDeploymentId]

instance ToPath GetDeployment where
        toPath = const "/"

instance ToQuery GetDeployment where
        toQuery = const mempty

-- | Represents the output of a get deployment operation.
--
-- /See:/ 'getDeploymentResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdrsDeploymentInfo'
--
-- * 'gdrsStatus'
data GetDeploymentResponse = GetDeploymentResponse'
    { _gdrsDeploymentInfo :: !(Maybe DeploymentInfo)
    , _gdrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDeploymentResponse' smart constructor.
getDeploymentResponse :: Int -> GetDeploymentResponse
getDeploymentResponse pStatus_ =
    GetDeploymentResponse'
    { _gdrsDeploymentInfo = Nothing
    , _gdrsStatus = pStatus_
    }

-- | Information about the deployment.
gdrsDeploymentInfo :: Lens' GetDeploymentResponse (Maybe DeploymentInfo)
gdrsDeploymentInfo = lens _gdrsDeploymentInfo (\ s a -> s{_gdrsDeploymentInfo = a});

-- | FIXME: Undocumented member.
gdrsStatus :: Lens' GetDeploymentResponse Int
gdrsStatus = lens _gdrsStatus (\ s a -> s{_gdrsStatus = a});

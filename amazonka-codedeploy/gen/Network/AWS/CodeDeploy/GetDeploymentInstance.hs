{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.GetDeploymentInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an instance as part of a deployment.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_GetDeploymentInstance.html>
module Network.AWS.CodeDeploy.GetDeploymentInstance
    (
    -- * Request
      GetDeploymentInstance
    -- ** Request constructor
    , getDeploymentInstance
    -- ** Request lenses
    , gdiDeploymentId
    , gdiInstanceId

    -- * Response
    , GetDeploymentInstanceResponse
    -- ** Response constructor
    , getDeploymentInstanceResponse
    -- ** Response lenses
    , gdirsInstanceSummary
    , gdirsStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a get deployment instance operation.
--
-- /See:/ 'getDeploymentInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdiDeploymentId'
--
-- * 'gdiInstanceId'
data GetDeploymentInstance = GetDeploymentInstance'
    { _gdiDeploymentId :: !Text
    , _gdiInstanceId   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDeploymentInstance' smart constructor.
getDeploymentInstance :: Text -> Text -> GetDeploymentInstance
getDeploymentInstance pDeploymentId_ pInstanceId_ =
    GetDeploymentInstance'
    { _gdiDeploymentId = pDeploymentId_
    , _gdiInstanceId = pInstanceId_
    }

-- | The unique ID of a deployment.
gdiDeploymentId :: Lens' GetDeploymentInstance Text
gdiDeploymentId = lens _gdiDeploymentId (\ s a -> s{_gdiDeploymentId = a});

-- | The unique ID of an instance in the deployment\'s deployment group.
gdiInstanceId :: Lens' GetDeploymentInstance Text
gdiInstanceId = lens _gdiInstanceId (\ s a -> s{_gdiInstanceId = a});

instance AWSRequest GetDeploymentInstance where
        type Sv GetDeploymentInstance = CodeDeploy
        type Rs GetDeploymentInstance =
             GetDeploymentInstanceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetDeploymentInstanceResponse' <$>
                   (x .?> "instanceSummary") <*> (pure (fromEnum s)))

instance ToHeaders GetDeploymentInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.GetDeploymentInstance" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDeploymentInstance where
        toJSON GetDeploymentInstance'{..}
          = object
              ["deploymentId" .= _gdiDeploymentId,
               "instanceId" .= _gdiInstanceId]

instance ToPath GetDeploymentInstance where
        toPath = const mempty

instance ToQuery GetDeploymentInstance where
        toQuery = const mempty

-- | Represents the output of a get deployment instance operation.
--
-- /See:/ 'getDeploymentInstanceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdirsInstanceSummary'
--
-- * 'gdirsStatus'
data GetDeploymentInstanceResponse = GetDeploymentInstanceResponse'
    { _gdirsInstanceSummary :: !(Maybe InstanceSummary)
    , _gdirsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDeploymentInstanceResponse' smart constructor.
getDeploymentInstanceResponse :: Int -> GetDeploymentInstanceResponse
getDeploymentInstanceResponse pStatus_ =
    GetDeploymentInstanceResponse'
    { _gdirsInstanceSummary = Nothing
    , _gdirsStatus = pStatus_
    }

-- | Information about the instance.
gdirsInstanceSummary :: Lens' GetDeploymentInstanceResponse (Maybe InstanceSummary)
gdirsInstanceSummary = lens _gdirsInstanceSummary (\ s a -> s{_gdirsInstanceSummary = a});

-- | FIXME: Undocumented member.
gdirsStatus :: Lens' GetDeploymentInstanceResponse Int
gdirsStatus = lens _gdirsStatus (\ s a -> s{_gdirsStatus = a});

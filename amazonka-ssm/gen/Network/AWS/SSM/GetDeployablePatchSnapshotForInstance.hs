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
-- Module      : Network.AWS.SSM.GetDeployablePatchSnapshotForInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current snapshot for the patch baseline the instance uses. This API is primarily used by the AWS-RunPatchBaseline Systems Manager document.
--
--
module Network.AWS.SSM.GetDeployablePatchSnapshotForInstance
    (
    -- * Creating a Request
      getDeployablePatchSnapshotForInstance
    , GetDeployablePatchSnapshotForInstance
    -- * Request Lenses
    , gdpsfiInstanceId
    , gdpsfiSnapshotId

    -- * Destructuring the Response
    , getDeployablePatchSnapshotForInstanceResponse
    , GetDeployablePatchSnapshotForInstanceResponse
    -- * Response Lenses
    , gdpsfirsInstanceId
    , gdpsfirsProduct
    , gdpsfirsSnapshotDownloadURL
    , gdpsfirsSnapshotId
    , gdpsfirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'getDeployablePatchSnapshotForInstance' smart constructor.
data GetDeployablePatchSnapshotForInstance = GetDeployablePatchSnapshotForInstance'
  { _gdpsfiInstanceId :: !Text
  , _gdpsfiSnapshotId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDeployablePatchSnapshotForInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdpsfiInstanceId' - The ID of the instance for which the appropriate patch snapshot should be retrieved.
--
-- * 'gdpsfiSnapshotId' - The user-defined snapshot ID.
getDeployablePatchSnapshotForInstance
    :: Text -- ^ 'gdpsfiInstanceId'
    -> Text -- ^ 'gdpsfiSnapshotId'
    -> GetDeployablePatchSnapshotForInstance
getDeployablePatchSnapshotForInstance pInstanceId_ pSnapshotId_ =
  GetDeployablePatchSnapshotForInstance'
    {_gdpsfiInstanceId = pInstanceId_, _gdpsfiSnapshotId = pSnapshotId_}


-- | The ID of the instance for which the appropriate patch snapshot should be retrieved.
gdpsfiInstanceId :: Lens' GetDeployablePatchSnapshotForInstance Text
gdpsfiInstanceId = lens _gdpsfiInstanceId (\ s a -> s{_gdpsfiInstanceId = a})

-- | The user-defined snapshot ID.
gdpsfiSnapshotId :: Lens' GetDeployablePatchSnapshotForInstance Text
gdpsfiSnapshotId = lens _gdpsfiSnapshotId (\ s a -> s{_gdpsfiSnapshotId = a})

instance AWSRequest
           GetDeployablePatchSnapshotForInstance
         where
        type Rs GetDeployablePatchSnapshotForInstance =
             GetDeployablePatchSnapshotForInstanceResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 GetDeployablePatchSnapshotForInstanceResponse' <$>
                   (x .?> "InstanceId") <*> (x .?> "Product") <*>
                     (x .?> "SnapshotDownloadUrl")
                     <*> (x .?> "SnapshotId")
                     <*> (pure (fromEnum s)))

instance Hashable
           GetDeployablePatchSnapshotForInstance
         where

instance NFData GetDeployablePatchSnapshotForInstance
         where

instance ToHeaders
           GetDeployablePatchSnapshotForInstance
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.GetDeployablePatchSnapshotForInstance" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDeployablePatchSnapshotForInstance
         where
        toJSON GetDeployablePatchSnapshotForInstance'{..}
          = object
              (catMaybes
                 [Just ("InstanceId" .= _gdpsfiInstanceId),
                  Just ("SnapshotId" .= _gdpsfiSnapshotId)])

instance ToPath GetDeployablePatchSnapshotForInstance
         where
        toPath = const "/"

instance ToQuery
           GetDeployablePatchSnapshotForInstance
         where
        toQuery = const mempty

-- | /See:/ 'getDeployablePatchSnapshotForInstanceResponse' smart constructor.
data GetDeployablePatchSnapshotForInstanceResponse = GetDeployablePatchSnapshotForInstanceResponse'
  { _gdpsfirsInstanceId          :: !(Maybe Text)
  , _gdpsfirsProduct             :: !(Maybe Text)
  , _gdpsfirsSnapshotDownloadURL :: !(Maybe Text)
  , _gdpsfirsSnapshotId          :: !(Maybe Text)
  , _gdpsfirsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDeployablePatchSnapshotForInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdpsfirsInstanceId' - The ID of the instance.
--
-- * 'gdpsfirsProduct' - Returns the specific operating system (for example Windows Server 2012 or Amazon Linux 2015.09) on the instance for the specified patch snapshot.
--
-- * 'gdpsfirsSnapshotDownloadURL' - A pre-signed Amazon S3 URL that can be used to download the patch snapshot.
--
-- * 'gdpsfirsSnapshotId' - The user-defined snapshot ID.
--
-- * 'gdpsfirsResponseStatus' - -- | The response status code.
getDeployablePatchSnapshotForInstanceResponse
    :: Int -- ^ 'gdpsfirsResponseStatus'
    -> GetDeployablePatchSnapshotForInstanceResponse
getDeployablePatchSnapshotForInstanceResponse pResponseStatus_ =
  GetDeployablePatchSnapshotForInstanceResponse'
    { _gdpsfirsInstanceId = Nothing
    , _gdpsfirsProduct = Nothing
    , _gdpsfirsSnapshotDownloadURL = Nothing
    , _gdpsfirsSnapshotId = Nothing
    , _gdpsfirsResponseStatus = pResponseStatus_
    }


-- | The ID of the instance.
gdpsfirsInstanceId :: Lens' GetDeployablePatchSnapshotForInstanceResponse (Maybe Text)
gdpsfirsInstanceId = lens _gdpsfirsInstanceId (\ s a -> s{_gdpsfirsInstanceId = a})

-- | Returns the specific operating system (for example Windows Server 2012 or Amazon Linux 2015.09) on the instance for the specified patch snapshot.
gdpsfirsProduct :: Lens' GetDeployablePatchSnapshotForInstanceResponse (Maybe Text)
gdpsfirsProduct = lens _gdpsfirsProduct (\ s a -> s{_gdpsfirsProduct = a})

-- | A pre-signed Amazon S3 URL that can be used to download the patch snapshot.
gdpsfirsSnapshotDownloadURL :: Lens' GetDeployablePatchSnapshotForInstanceResponse (Maybe Text)
gdpsfirsSnapshotDownloadURL = lens _gdpsfirsSnapshotDownloadURL (\ s a -> s{_gdpsfirsSnapshotDownloadURL = a})

-- | The user-defined snapshot ID.
gdpsfirsSnapshotId :: Lens' GetDeployablePatchSnapshotForInstanceResponse (Maybe Text)
gdpsfirsSnapshotId = lens _gdpsfirsSnapshotId (\ s a -> s{_gdpsfirsSnapshotId = a})

-- | -- | The response status code.
gdpsfirsResponseStatus :: Lens' GetDeployablePatchSnapshotForInstanceResponse Int
gdpsfirsResponseStatus = lens _gdpsfirsResponseStatus (\ s a -> s{_gdpsfirsResponseStatus = a})

instance NFData
           GetDeployablePatchSnapshotForInstanceResponse
         where

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
-- Module      : Network.AWS.CloudHSMv2.CopyBackupToRegion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copy an AWS CloudHSM cluster backup to a different region.
--
--
module Network.AWS.CloudHSMv2.CopyBackupToRegion
    (
    -- * Creating a Request
      copyBackupToRegion
    , CopyBackupToRegion
    -- * Request Lenses
    , cbtrDestinationRegion
    , cbtrBackupId

    -- * Destructuring the Response
    , copyBackupToRegionResponse
    , CopyBackupToRegionResponse
    -- * Response Lenses
    , cbtrrsDestinationBackup
    , cbtrrsResponseStatus
    ) where

import Network.AWS.CloudHSMv2.Types
import Network.AWS.CloudHSMv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'copyBackupToRegion' smart constructor.
data CopyBackupToRegion = CopyBackupToRegion'
  { _cbtrDestinationRegion :: !Text
  , _cbtrBackupId          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopyBackupToRegion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbtrDestinationRegion' - The AWS region that will contain your copied CloudHSM cluster backup.
--
-- * 'cbtrBackupId' - The ID of the backup that will be copied to the destination region.
copyBackupToRegion
    :: Text -- ^ 'cbtrDestinationRegion'
    -> Text -- ^ 'cbtrBackupId'
    -> CopyBackupToRegion
copyBackupToRegion pDestinationRegion_ pBackupId_ =
  CopyBackupToRegion'
    {_cbtrDestinationRegion = pDestinationRegion_, _cbtrBackupId = pBackupId_}


-- | The AWS region that will contain your copied CloudHSM cluster backup.
cbtrDestinationRegion :: Lens' CopyBackupToRegion Text
cbtrDestinationRegion = lens _cbtrDestinationRegion (\ s a -> s{_cbtrDestinationRegion = a})

-- | The ID of the backup that will be copied to the destination region.
cbtrBackupId :: Lens' CopyBackupToRegion Text
cbtrBackupId = lens _cbtrBackupId (\ s a -> s{_cbtrBackupId = a})

instance AWSRequest CopyBackupToRegion where
        type Rs CopyBackupToRegion =
             CopyBackupToRegionResponse
        request = postJSON cloudHSMv2
        response
          = receiveJSON
              (\ s h x ->
                 CopyBackupToRegionResponse' <$>
                   (x .?> "DestinationBackup") <*> (pure (fromEnum s)))

instance Hashable CopyBackupToRegion where

instance NFData CopyBackupToRegion where

instance ToHeaders CopyBackupToRegion where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("BaldrApiService.CopyBackupToRegion" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CopyBackupToRegion where
        toJSON CopyBackupToRegion'{..}
          = object
              (catMaybes
                 [Just
                    ("DestinationRegion" .= _cbtrDestinationRegion),
                  Just ("BackupId" .= _cbtrBackupId)])

instance ToPath CopyBackupToRegion where
        toPath = const "/"

instance ToQuery CopyBackupToRegion where
        toQuery = const mempty

-- | /See:/ 'copyBackupToRegionResponse' smart constructor.
data CopyBackupToRegionResponse = CopyBackupToRegionResponse'
  { _cbtrrsDestinationBackup :: !(Maybe DestinationBackup)
  , _cbtrrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopyBackupToRegionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbtrrsDestinationBackup' - Information on the backup that will be copied to the destination region, including CreateTimestamp, SourceBackup, SourceCluster, and Source Region. CreateTimestamp of the destination backup will be the same as that of the source backup. You will need to use the @sourceBackupID@ returned in this operation to use the 'DescribeBackups' operation on the backup that will be copied to the destination region.
--
-- * 'cbtrrsResponseStatus' - -- | The response status code.
copyBackupToRegionResponse
    :: Int -- ^ 'cbtrrsResponseStatus'
    -> CopyBackupToRegionResponse
copyBackupToRegionResponse pResponseStatus_ =
  CopyBackupToRegionResponse'
    { _cbtrrsDestinationBackup = Nothing
    , _cbtrrsResponseStatus = pResponseStatus_
    }


-- | Information on the backup that will be copied to the destination region, including CreateTimestamp, SourceBackup, SourceCluster, and Source Region. CreateTimestamp of the destination backup will be the same as that of the source backup. You will need to use the @sourceBackupID@ returned in this operation to use the 'DescribeBackups' operation on the backup that will be copied to the destination region.
cbtrrsDestinationBackup :: Lens' CopyBackupToRegionResponse (Maybe DestinationBackup)
cbtrrsDestinationBackup = lens _cbtrrsDestinationBackup (\ s a -> s{_cbtrrsDestinationBackup = a})

-- | -- | The response status code.
cbtrrsResponseStatus :: Lens' CopyBackupToRegionResponse Int
cbtrrsResponseStatus = lens _cbtrrsResponseStatus (\ s a -> s{_cbtrrsResponseStatus = a})

instance NFData CopyBackupToRegionResponse where

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
-- Module      : Network.AWS.SMS.CreateReplicationJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The CreateReplicationJob API is used to create a ReplicationJob to replicate a server on AWS. Call this API to first create a ReplicationJob, which will then schedule periodic ReplicationRuns to replicate your server to AWS. Each ReplicationRun will result in the creation of an AWS AMI.
module Network.AWS.SMS.CreateReplicationJob
    (
    -- * Creating a Request
      createReplicationJob
    , CreateReplicationJob
    -- * Request Lenses
    , crjLicenseType
    , crjRoleName
    , crjDescription
    , crjServerId
    , crjSeedReplicationTime
    , crjFrequency

    -- * Destructuring the Response
    , createReplicationJobResponse
    , CreateReplicationJobResponse
    -- * Response Lenses
    , crjrsReplicationJobId
    , crjrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types
import Network.AWS.SMS.Types.Product

-- | /See:/ 'createReplicationJob' smart constructor.
data CreateReplicationJob = CreateReplicationJob'
  { _crjLicenseType         :: !(Maybe LicenseType)
  , _crjRoleName            :: !(Maybe Text)
  , _crjDescription         :: !(Maybe Text)
  , _crjServerId            :: !Text
  , _crjSeedReplicationTime :: !POSIX
  , _crjFrequency           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateReplicationJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crjLicenseType' - Undocumented member.
--
-- * 'crjRoleName' - Undocumented member.
--
-- * 'crjDescription' - Undocumented member.
--
-- * 'crjServerId' - Undocumented member.
--
-- * 'crjSeedReplicationTime' - Undocumented member.
--
-- * 'crjFrequency' - Undocumented member.
createReplicationJob
    :: Text -- ^ 'crjServerId'
    -> UTCTime -- ^ 'crjSeedReplicationTime'
    -> Int -- ^ 'crjFrequency'
    -> CreateReplicationJob
createReplicationJob pServerId_ pSeedReplicationTime_ pFrequency_ =
  CreateReplicationJob'
    { _crjLicenseType = Nothing
    , _crjRoleName = Nothing
    , _crjDescription = Nothing
    , _crjServerId = pServerId_
    , _crjSeedReplicationTime = _Time # pSeedReplicationTime_
    , _crjFrequency = pFrequency_
    }


-- | Undocumented member.
crjLicenseType :: Lens' CreateReplicationJob (Maybe LicenseType)
crjLicenseType = lens _crjLicenseType (\ s a -> s{_crjLicenseType = a})

-- | Undocumented member.
crjRoleName :: Lens' CreateReplicationJob (Maybe Text)
crjRoleName = lens _crjRoleName (\ s a -> s{_crjRoleName = a})

-- | Undocumented member.
crjDescription :: Lens' CreateReplicationJob (Maybe Text)
crjDescription = lens _crjDescription (\ s a -> s{_crjDescription = a})

-- | Undocumented member.
crjServerId :: Lens' CreateReplicationJob Text
crjServerId = lens _crjServerId (\ s a -> s{_crjServerId = a})

-- | Undocumented member.
crjSeedReplicationTime :: Lens' CreateReplicationJob UTCTime
crjSeedReplicationTime = lens _crjSeedReplicationTime (\ s a -> s{_crjSeedReplicationTime = a}) . _Time

-- | Undocumented member.
crjFrequency :: Lens' CreateReplicationJob Int
crjFrequency = lens _crjFrequency (\ s a -> s{_crjFrequency = a})

instance AWSRequest CreateReplicationJob where
        type Rs CreateReplicationJob =
             CreateReplicationJobResponse
        request = postJSON sms
        response
          = receiveJSON
              (\ s h x ->
                 CreateReplicationJobResponse' <$>
                   (x .?> "replicationJobId") <*> (pure (fromEnum s)))

instance Hashable CreateReplicationJob where

instance NFData CreateReplicationJob where

instance ToHeaders CreateReplicationJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSServerMigrationService_V2016_10_24.CreateReplicationJob"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateReplicationJob where
        toJSON CreateReplicationJob'{..}
          = object
              (catMaybes
                 [("licenseType" .=) <$> _crjLicenseType,
                  ("roleName" .=) <$> _crjRoleName,
                  ("description" .=) <$> _crjDescription,
                  Just ("serverId" .= _crjServerId),
                  Just
                    ("seedReplicationTime" .= _crjSeedReplicationTime),
                  Just ("frequency" .= _crjFrequency)])

instance ToPath CreateReplicationJob where
        toPath = const "/"

instance ToQuery CreateReplicationJob where
        toQuery = const mempty

-- | /See:/ 'createReplicationJobResponse' smart constructor.
data CreateReplicationJobResponse = CreateReplicationJobResponse'
  { _crjrsReplicationJobId :: !(Maybe Text)
  , _crjrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateReplicationJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crjrsReplicationJobId' - Undocumented member.
--
-- * 'crjrsResponseStatus' - -- | The response status code.
createReplicationJobResponse
    :: Int -- ^ 'crjrsResponseStatus'
    -> CreateReplicationJobResponse
createReplicationJobResponse pResponseStatus_ =
  CreateReplicationJobResponse'
    {_crjrsReplicationJobId = Nothing, _crjrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
crjrsReplicationJobId :: Lens' CreateReplicationJobResponse (Maybe Text)
crjrsReplicationJobId = lens _crjrsReplicationJobId (\ s a -> s{_crjrsReplicationJobId = a})

-- | -- | The response status code.
crjrsResponseStatus :: Lens' CreateReplicationJobResponse Int
crjrsResponseStatus = lens _crjrsResponseStatus (\ s a -> s{_crjrsResponseStatus = a})

instance NFData CreateReplicationJobResponse where

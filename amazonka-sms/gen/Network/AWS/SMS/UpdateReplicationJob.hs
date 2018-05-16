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
-- Module      : Network.AWS.SMS.UpdateReplicationJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The UpdateReplicationJob API is used to change the settings of your existing ReplicationJob created using CreateReplicationJob. Calling this API will affect the next scheduled ReplicationRun.
module Network.AWS.SMS.UpdateReplicationJob
    (
    -- * Creating a Request
      updateReplicationJob
    , UpdateReplicationJob
    -- * Request Lenses
    , urjFrequency
    , urjLicenseType
    , urjRoleName
    , urjNextReplicationRunStartTime
    , urjDescription
    , urjReplicationJobId

    -- * Destructuring the Response
    , updateReplicationJobResponse
    , UpdateReplicationJobResponse
    -- * Response Lenses
    , urjrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types
import Network.AWS.SMS.Types.Product

-- | /See:/ 'updateReplicationJob' smart constructor.
data UpdateReplicationJob = UpdateReplicationJob'
  { _urjFrequency                   :: !(Maybe Int)
  , _urjLicenseType                 :: !(Maybe LicenseType)
  , _urjRoleName                    :: !(Maybe Text)
  , _urjNextReplicationRunStartTime :: !(Maybe POSIX)
  , _urjDescription                 :: !(Maybe Text)
  , _urjReplicationJobId            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateReplicationJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urjFrequency' - Undocumented member.
--
-- * 'urjLicenseType' - Undocumented member.
--
-- * 'urjRoleName' - Undocumented member.
--
-- * 'urjNextReplicationRunStartTime' - Undocumented member.
--
-- * 'urjDescription' - Undocumented member.
--
-- * 'urjReplicationJobId' - Undocumented member.
updateReplicationJob
    :: Text -- ^ 'urjReplicationJobId'
    -> UpdateReplicationJob
updateReplicationJob pReplicationJobId_ =
  UpdateReplicationJob'
    { _urjFrequency = Nothing
    , _urjLicenseType = Nothing
    , _urjRoleName = Nothing
    , _urjNextReplicationRunStartTime = Nothing
    , _urjDescription = Nothing
    , _urjReplicationJobId = pReplicationJobId_
    }


-- | Undocumented member.
urjFrequency :: Lens' UpdateReplicationJob (Maybe Int)
urjFrequency = lens _urjFrequency (\ s a -> s{_urjFrequency = a})

-- | Undocumented member.
urjLicenseType :: Lens' UpdateReplicationJob (Maybe LicenseType)
urjLicenseType = lens _urjLicenseType (\ s a -> s{_urjLicenseType = a})

-- | Undocumented member.
urjRoleName :: Lens' UpdateReplicationJob (Maybe Text)
urjRoleName = lens _urjRoleName (\ s a -> s{_urjRoleName = a})

-- | Undocumented member.
urjNextReplicationRunStartTime :: Lens' UpdateReplicationJob (Maybe UTCTime)
urjNextReplicationRunStartTime = lens _urjNextReplicationRunStartTime (\ s a -> s{_urjNextReplicationRunStartTime = a}) . mapping _Time

-- | Undocumented member.
urjDescription :: Lens' UpdateReplicationJob (Maybe Text)
urjDescription = lens _urjDescription (\ s a -> s{_urjDescription = a})

-- | Undocumented member.
urjReplicationJobId :: Lens' UpdateReplicationJob Text
urjReplicationJobId = lens _urjReplicationJobId (\ s a -> s{_urjReplicationJobId = a})

instance AWSRequest UpdateReplicationJob where
        type Rs UpdateReplicationJob =
             UpdateReplicationJobResponse
        request = postJSON sms
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateReplicationJobResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateReplicationJob where

instance NFData UpdateReplicationJob where

instance ToHeaders UpdateReplicationJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSServerMigrationService_V2016_10_24.UpdateReplicationJob"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateReplicationJob where
        toJSON UpdateReplicationJob'{..}
          = object
              (catMaybes
                 [("frequency" .=) <$> _urjFrequency,
                  ("licenseType" .=) <$> _urjLicenseType,
                  ("roleName" .=) <$> _urjRoleName,
                  ("nextReplicationRunStartTime" .=) <$>
                    _urjNextReplicationRunStartTime,
                  ("description" .=) <$> _urjDescription,
                  Just ("replicationJobId" .= _urjReplicationJobId)])

instance ToPath UpdateReplicationJob where
        toPath = const "/"

instance ToQuery UpdateReplicationJob where
        toQuery = const mempty

-- | /See:/ 'updateReplicationJobResponse' smart constructor.
newtype UpdateReplicationJobResponse = UpdateReplicationJobResponse'
  { _urjrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateReplicationJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urjrsResponseStatus' - -- | The response status code.
updateReplicationJobResponse
    :: Int -- ^ 'urjrsResponseStatus'
    -> UpdateReplicationJobResponse
updateReplicationJobResponse pResponseStatus_ =
  UpdateReplicationJobResponse' {_urjrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
urjrsResponseStatus :: Lens' UpdateReplicationJobResponse Int
urjrsResponseStatus = lens _urjrsResponseStatus (\ s a -> s{_urjrsResponseStatus = a})

instance NFData UpdateReplicationJobResponse where

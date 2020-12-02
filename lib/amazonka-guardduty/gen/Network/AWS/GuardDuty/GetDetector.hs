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
-- Module      : Network.AWS.GuardDuty.GetDetector
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an Amazon GuardDuty detector specified by the detectorId.
module Network.AWS.GuardDuty.GetDetector
    (
    -- * Creating a Request
      getDetector
    , GetDetector
    -- * Request Lenses
    , gdDetectorId

    -- * Destructuring the Response
    , getDetectorResponse
    , GetDetectorResponse
    -- * Response Lenses
    , gdrsStatus
    , gdrsCreatedAt
    , gdrsUpdatedAt
    , gdrsServiceRole
    , gdrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDetector' smart constructor.
newtype GetDetector = GetDetector'
  { _gdDetectorId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDetector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdDetectorId' - The unique ID of the detector that you want to retrieve.
getDetector
    :: Text -- ^ 'gdDetectorId'
    -> GetDetector
getDetector pDetectorId_ = GetDetector' {_gdDetectorId = pDetectorId_}


-- | The unique ID of the detector that you want to retrieve.
gdDetectorId :: Lens' GetDetector Text
gdDetectorId = lens _gdDetectorId (\ s a -> s{_gdDetectorId = a})

instance AWSRequest GetDetector where
        type Rs GetDetector = GetDetectorResponse
        request = get guardDuty
        response
          = receiveJSON
              (\ s h x ->
                 GetDetectorResponse' <$>
                   (x .?> "status") <*> (x .?> "createdAt") <*>
                     (x .?> "updatedAt")
                     <*> (x .?> "serviceRole")
                     <*> (pure (fromEnum s)))

instance Hashable GetDetector where

instance NFData GetDetector where

instance ToHeaders GetDetector where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetDetector where
        toPath GetDetector'{..}
          = mconcat ["/detector/", toBS _gdDetectorId]

instance ToQuery GetDetector where
        toQuery = const mempty

-- | /See:/ 'getDetectorResponse' smart constructor.
data GetDetectorResponse = GetDetectorResponse'
  { _gdrsStatus         :: !(Maybe DetectorStatus)
  , _gdrsCreatedAt      :: !(Maybe Text)
  , _gdrsUpdatedAt      :: !(Maybe Text)
  , _gdrsServiceRole    :: !(Maybe Text)
  , _gdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDetectorResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrsStatus' - Undocumented member.
--
-- * 'gdrsCreatedAt' - Undocumented member.
--
-- * 'gdrsUpdatedAt' - Undocumented member.
--
-- * 'gdrsServiceRole' - Undocumented member.
--
-- * 'gdrsResponseStatus' - -- | The response status code.
getDetectorResponse
    :: Int -- ^ 'gdrsResponseStatus'
    -> GetDetectorResponse
getDetectorResponse pResponseStatus_ =
  GetDetectorResponse'
    { _gdrsStatus = Nothing
    , _gdrsCreatedAt = Nothing
    , _gdrsUpdatedAt = Nothing
    , _gdrsServiceRole = Nothing
    , _gdrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
gdrsStatus :: Lens' GetDetectorResponse (Maybe DetectorStatus)
gdrsStatus = lens _gdrsStatus (\ s a -> s{_gdrsStatus = a})

-- | Undocumented member.
gdrsCreatedAt :: Lens' GetDetectorResponse (Maybe Text)
gdrsCreatedAt = lens _gdrsCreatedAt (\ s a -> s{_gdrsCreatedAt = a})

-- | Undocumented member.
gdrsUpdatedAt :: Lens' GetDetectorResponse (Maybe Text)
gdrsUpdatedAt = lens _gdrsUpdatedAt (\ s a -> s{_gdrsUpdatedAt = a})

-- | Undocumented member.
gdrsServiceRole :: Lens' GetDetectorResponse (Maybe Text)
gdrsServiceRole = lens _gdrsServiceRole (\ s a -> s{_gdrsServiceRole = a})

-- | -- | The response status code.
gdrsResponseStatus :: Lens' GetDetectorResponse Int
gdrsResponseStatus = lens _gdrsResponseStatus (\ s a -> s{_gdrsResponseStatus = a})

instance NFData GetDetectorResponse where

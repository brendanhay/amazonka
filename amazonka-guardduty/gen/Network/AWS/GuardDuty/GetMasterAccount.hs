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
-- Module      : Network.AWS.GuardDuty.GetMasterAccount
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the details for the GuardDuty master account to the current GuardDuty member account.
module Network.AWS.GuardDuty.GetMasterAccount
    (
    -- * Creating a Request
      getMasterAccount
    , GetMasterAccount
    -- * Request Lenses
    , gmaDetectorId

    -- * Destructuring the Response
    , getMasterAccountResponse
    , GetMasterAccountResponse
    -- * Response Lenses
    , gmarsMaster
    , gmarsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getMasterAccount' smart constructor.
newtype GetMasterAccount = GetMasterAccount'
  { _gmaDetectorId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMasterAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmaDetectorId' - The unique ID of the detector of the GuardDuty member account.
getMasterAccount
    :: Text -- ^ 'gmaDetectorId'
    -> GetMasterAccount
getMasterAccount pDetectorId_ =
  GetMasterAccount' {_gmaDetectorId = pDetectorId_}


-- | The unique ID of the detector of the GuardDuty member account.
gmaDetectorId :: Lens' GetMasterAccount Text
gmaDetectorId = lens _gmaDetectorId (\ s a -> s{_gmaDetectorId = a})

instance AWSRequest GetMasterAccount where
        type Rs GetMasterAccount = GetMasterAccountResponse
        request = get guardDuty
        response
          = receiveJSON
              (\ s h x ->
                 GetMasterAccountResponse' <$>
                   (x .?> "master") <*> (pure (fromEnum s)))

instance Hashable GetMasterAccount where

instance NFData GetMasterAccount where

instance ToHeaders GetMasterAccount where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetMasterAccount where
        toPath GetMasterAccount'{..}
          = mconcat
              ["/detector/", toBS _gmaDetectorId, "/master"]

instance ToQuery GetMasterAccount where
        toQuery = const mempty

-- | /See:/ 'getMasterAccountResponse' smart constructor.
data GetMasterAccountResponse = GetMasterAccountResponse'
  { _gmarsMaster         :: !(Maybe Master)
  , _gmarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMasterAccountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmarsMaster' - Undocumented member.
--
-- * 'gmarsResponseStatus' - -- | The response status code.
getMasterAccountResponse
    :: Int -- ^ 'gmarsResponseStatus'
    -> GetMasterAccountResponse
getMasterAccountResponse pResponseStatus_ =
  GetMasterAccountResponse'
    {_gmarsMaster = Nothing, _gmarsResponseStatus = pResponseStatus_}


-- | Undocumented member.
gmarsMaster :: Lens' GetMasterAccountResponse (Maybe Master)
gmarsMaster = lens _gmarsMaster (\ s a -> s{_gmarsMaster = a})

-- | -- | The response status code.
gmarsResponseStatus :: Lens' GetMasterAccountResponse Int
gmarsResponseStatus = lens _gmarsResponseStatus (\ s a -> s{_gmarsResponseStatus = a})

instance NFData GetMasterAccountResponse where

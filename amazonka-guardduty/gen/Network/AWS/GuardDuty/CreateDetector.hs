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
-- Module      : Network.AWS.GuardDuty.CreateDetector
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a single Amazon GuardDuty detector. A detector is an object that represents the GuardDuty service. A detector must be created in order for GuardDuty to become operational.
module Network.AWS.GuardDuty.CreateDetector
    (
    -- * Creating a Request
      createDetector
    , CreateDetector
    -- * Request Lenses
    , cdEnable

    -- * Destructuring the Response
    , createDetectorResponse
    , CreateDetectorResponse
    -- * Response Lenses
    , cdrsDetectorId
    , cdrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | CreateDetector request body.
--
-- /See:/ 'createDetector' smart constructor.
newtype CreateDetector = CreateDetector'
  { _cdEnable :: Maybe Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDetector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdEnable' - A boolean value that specifies whether the detector is to be enabled.
createDetector
    :: CreateDetector
createDetector = CreateDetector' {_cdEnable = Nothing}


-- | A boolean value that specifies whether the detector is to be enabled.
cdEnable :: Lens' CreateDetector (Maybe Bool)
cdEnable = lens _cdEnable (\ s a -> s{_cdEnable = a})

instance AWSRequest CreateDetector where
        type Rs CreateDetector = CreateDetectorResponse
        request = postJSON guardDuty
        response
          = receiveJSON
              (\ s h x ->
                 CreateDetectorResponse' <$>
                   (x .?> "detectorId") <*> (pure (fromEnum s)))

instance Hashable CreateDetector where

instance NFData CreateDetector where

instance ToHeaders CreateDetector where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDetector where
        toJSON CreateDetector'{..}
          = object (catMaybes [("enable" .=) <$> _cdEnable])

instance ToPath CreateDetector where
        toPath = const "/detector"

instance ToQuery CreateDetector where
        toQuery = const mempty

-- | /See:/ 'createDetectorResponse' smart constructor.
data CreateDetectorResponse = CreateDetectorResponse'
  { _cdrsDetectorId     :: !(Maybe Text)
  , _cdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDetectorResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdrsDetectorId' - The unique ID of the created detector.
--
-- * 'cdrsResponseStatus' - -- | The response status code.
createDetectorResponse
    :: Int -- ^ 'cdrsResponseStatus'
    -> CreateDetectorResponse
createDetectorResponse pResponseStatus_ =
  CreateDetectorResponse'
    {_cdrsDetectorId = Nothing, _cdrsResponseStatus = pResponseStatus_}


-- | The unique ID of the created detector.
cdrsDetectorId :: Lens' CreateDetectorResponse (Maybe Text)
cdrsDetectorId = lens _cdrsDetectorId (\ s a -> s{_cdrsDetectorId = a})

-- | -- | The response status code.
cdrsResponseStatus :: Lens' CreateDetectorResponse Int
cdrsResponseStatus = lens _cdrsResponseStatus (\ s a -> s{_cdrsResponseStatus = a})

instance NFData CreateDetectorResponse where

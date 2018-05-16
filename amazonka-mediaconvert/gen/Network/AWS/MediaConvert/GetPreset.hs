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
-- Module      : Network.AWS.MediaConvert.GetPreset
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve the JSON for a specific preset.
module Network.AWS.MediaConvert.GetPreset
    (
    -- * Creating a Request
      getPreset
    , GetPreset
    -- * Request Lenses
    , gpName

    -- * Destructuring the Response
    , getPresetResponse
    , GetPresetResponse
    -- * Response Lenses
    , gprsPreset
    , gprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.MediaConvert.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getPreset' smart constructor.
newtype GetPreset = GetPreset'
  { _gpName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPreset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpName' - The name of the preset.
getPreset
    :: Text -- ^ 'gpName'
    -> GetPreset
getPreset pName_ = GetPreset' {_gpName = pName_}


-- | The name of the preset.
gpName :: Lens' GetPreset Text
gpName = lens _gpName (\ s a -> s{_gpName = a})

instance AWSRequest GetPreset where
        type Rs GetPreset = GetPresetResponse
        request = get mediaConvert
        response
          = receiveJSON
              (\ s h x ->
                 GetPresetResponse' <$>
                   (x .?> "preset") <*> (pure (fromEnum s)))

instance Hashable GetPreset where

instance NFData GetPreset where

instance ToHeaders GetPreset where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetPreset where
        toPath GetPreset'{..}
          = mconcat ["/2017-08-29/presets/", toBS _gpName]

instance ToQuery GetPreset where
        toQuery = const mempty

-- | /See:/ 'getPresetResponse' smart constructor.
data GetPresetResponse = GetPresetResponse'
  { _gprsPreset         :: !(Maybe Preset)
  , _gprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPresetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gprsPreset' - Undocumented member.
--
-- * 'gprsResponseStatus' - -- | The response status code.
getPresetResponse
    :: Int -- ^ 'gprsResponseStatus'
    -> GetPresetResponse
getPresetResponse pResponseStatus_ =
  GetPresetResponse'
    {_gprsPreset = Nothing, _gprsResponseStatus = pResponseStatus_}


-- | Undocumented member.
gprsPreset :: Lens' GetPresetResponse (Maybe Preset)
gprsPreset = lens _gprsPreset (\ s a -> s{_gprsPreset = a})

-- | -- | The response status code.
gprsResponseStatus :: Lens' GetPresetResponse Int
gprsResponseStatus = lens _gprsResponseStatus (\ s a -> s{_gprsResponseStatus = a})

instance NFData GetPresetResponse where

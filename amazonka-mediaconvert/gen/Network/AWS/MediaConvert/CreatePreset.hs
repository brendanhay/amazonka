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
-- Module      : Network.AWS.MediaConvert.CreatePreset
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new preset. For information about job templates see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
module Network.AWS.MediaConvert.CreatePreset
    (
    -- * Creating a Request
      createPreset
    , CreatePreset
    -- * Request Lenses
    , cpSettings
    , cpCategory
    , cpName
    , cpDescription

    -- * Destructuring the Response
    , createPresetResponse
    , CreatePresetResponse
    -- * Response Lenses
    , cprsPreset
    , cprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.MediaConvert.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createPreset' smart constructor.
data CreatePreset = CreatePreset'
  { _cpSettings    :: !(Maybe PresetSettings)
  , _cpCategory    :: !(Maybe Text)
  , _cpName        :: !(Maybe Text)
  , _cpDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePreset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpSettings' - Undocumented member.
--
-- * 'cpCategory' - Optional. A category for the preset you are creating.
--
-- * 'cpName' - The name of the preset you are creating.
--
-- * 'cpDescription' - Optional. A description of the preset you are creating.
createPreset
    :: CreatePreset
createPreset =
  CreatePreset'
    { _cpSettings = Nothing
    , _cpCategory = Nothing
    , _cpName = Nothing
    , _cpDescription = Nothing
    }


-- | Undocumented member.
cpSettings :: Lens' CreatePreset (Maybe PresetSettings)
cpSettings = lens _cpSettings (\ s a -> s{_cpSettings = a})

-- | Optional. A category for the preset you are creating.
cpCategory :: Lens' CreatePreset (Maybe Text)
cpCategory = lens _cpCategory (\ s a -> s{_cpCategory = a})

-- | The name of the preset you are creating.
cpName :: Lens' CreatePreset (Maybe Text)
cpName = lens _cpName (\ s a -> s{_cpName = a})

-- | Optional. A description of the preset you are creating.
cpDescription :: Lens' CreatePreset (Maybe Text)
cpDescription = lens _cpDescription (\ s a -> s{_cpDescription = a})

instance AWSRequest CreatePreset where
        type Rs CreatePreset = CreatePresetResponse
        request = postJSON mediaConvert
        response
          = receiveJSON
              (\ s h x ->
                 CreatePresetResponse' <$>
                   (x .?> "preset") <*> (pure (fromEnum s)))

instance Hashable CreatePreset where

instance NFData CreatePreset where

instance ToHeaders CreatePreset where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreatePreset where
        toJSON CreatePreset'{..}
          = object
              (catMaybes
                 [("settings" .=) <$> _cpSettings,
                  ("category" .=) <$> _cpCategory,
                  ("name" .=) <$> _cpName,
                  ("description" .=) <$> _cpDescription])

instance ToPath CreatePreset where
        toPath = const "/2017-08-29/presets"

instance ToQuery CreatePreset where
        toQuery = const mempty

-- | /See:/ 'createPresetResponse' smart constructor.
data CreatePresetResponse = CreatePresetResponse'
  { _cprsPreset         :: !(Maybe Preset)
  , _cprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePresetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsPreset' - Undocumented member.
--
-- * 'cprsResponseStatus' - -- | The response status code.
createPresetResponse
    :: Int -- ^ 'cprsResponseStatus'
    -> CreatePresetResponse
createPresetResponse pResponseStatus_ =
  CreatePresetResponse'
    {_cprsPreset = Nothing, _cprsResponseStatus = pResponseStatus_}


-- | Undocumented member.
cprsPreset :: Lens' CreatePresetResponse (Maybe Preset)
cprsPreset = lens _cprsPreset (\ s a -> s{_cprsPreset = a})

-- | -- | The response status code.
cprsResponseStatus :: Lens' CreatePresetResponse Int
cprsResponseStatus = lens _cprsResponseStatus (\ s a -> s{_cprsResponseStatus = a})

instance NFData CreatePresetResponse where

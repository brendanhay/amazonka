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
-- Module      : Network.AWS.MediaConvert.UpdatePreset
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify one of your existing presets.
module Network.AWS.MediaConvert.UpdatePreset
    (
    -- * Creating a Request
      updatePreset
    , UpdatePreset
    -- * Request Lenses
    , upSettings
    , upCategory
    , upDescription
    , upName

    -- * Destructuring the Response
    , updatePresetResponse
    , UpdatePresetResponse
    -- * Response Lenses
    , uprsPreset
    , uprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.MediaConvert.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updatePreset' smart constructor.
data UpdatePreset = UpdatePreset'
  { _upSettings    :: !(Maybe PresetSettings)
  , _upCategory    :: !(Maybe Text)
  , _upDescription :: !(Maybe Text)
  , _upName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdatePreset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upSettings' - Undocumented member.
--
-- * 'upCategory' - The new category for the preset, if you are changing it.
--
-- * 'upDescription' - The new description for the preset, if you are changing it.
--
-- * 'upName' - The name of the preset you are modifying.
updatePreset
    :: Text -- ^ 'upName'
    -> UpdatePreset
updatePreset pName_ =
  UpdatePreset'
    { _upSettings = Nothing
    , _upCategory = Nothing
    , _upDescription = Nothing
    , _upName = pName_
    }


-- | Undocumented member.
upSettings :: Lens' UpdatePreset (Maybe PresetSettings)
upSettings = lens _upSettings (\ s a -> s{_upSettings = a})

-- | The new category for the preset, if you are changing it.
upCategory :: Lens' UpdatePreset (Maybe Text)
upCategory = lens _upCategory (\ s a -> s{_upCategory = a})

-- | The new description for the preset, if you are changing it.
upDescription :: Lens' UpdatePreset (Maybe Text)
upDescription = lens _upDescription (\ s a -> s{_upDescription = a})

-- | The name of the preset you are modifying.
upName :: Lens' UpdatePreset Text
upName = lens _upName (\ s a -> s{_upName = a})

instance AWSRequest UpdatePreset where
        type Rs UpdatePreset = UpdatePresetResponse
        request = putJSON mediaConvert
        response
          = receiveJSON
              (\ s h x ->
                 UpdatePresetResponse' <$>
                   (x .?> "preset") <*> (pure (fromEnum s)))

instance Hashable UpdatePreset where

instance NFData UpdatePreset where

instance ToHeaders UpdatePreset where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdatePreset where
        toJSON UpdatePreset'{..}
          = object
              (catMaybes
                 [("settings" .=) <$> _upSettings,
                  ("category" .=) <$> _upCategory,
                  ("description" .=) <$> _upDescription])

instance ToPath UpdatePreset where
        toPath UpdatePreset'{..}
          = mconcat ["/2017-08-29/presets/", toBS _upName]

instance ToQuery UpdatePreset where
        toQuery = const mempty

-- | /See:/ 'updatePresetResponse' smart constructor.
data UpdatePresetResponse = UpdatePresetResponse'
  { _uprsPreset         :: !(Maybe Preset)
  , _uprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdatePresetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uprsPreset' - Undocumented member.
--
-- * 'uprsResponseStatus' - -- | The response status code.
updatePresetResponse
    :: Int -- ^ 'uprsResponseStatus'
    -> UpdatePresetResponse
updatePresetResponse pResponseStatus_ =
  UpdatePresetResponse'
    {_uprsPreset = Nothing, _uprsResponseStatus = pResponseStatus_}


-- | Undocumented member.
uprsPreset :: Lens' UpdatePresetResponse (Maybe Preset)
uprsPreset = lens _uprsPreset (\ s a -> s{_uprsPreset = a})

-- | -- | The response status code.
uprsResponseStatus :: Lens' UpdatePresetResponse Int
uprsResponseStatus = lens _uprsResponseStatus (\ s a -> s{_uprsResponseStatus = a})

instance NFData UpdatePresetResponse where

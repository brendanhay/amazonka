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
-- Module      : Network.AWS.MediaConvert.DeletePreset
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently delete a preset you have created.
module Network.AWS.MediaConvert.DeletePreset
    (
    -- * Creating a Request
      deletePreset
    , DeletePreset
    -- * Request Lenses
    , dpName

    -- * Destructuring the Response
    , deletePresetResponse
    , DeletePresetResponse
    -- * Response Lenses
    , dprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.MediaConvert.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deletePreset' smart constructor.
newtype DeletePreset = DeletePreset'
  { _dpName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePreset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpName' - The name of the preset to be deleted.
deletePreset
    :: Text -- ^ 'dpName'
    -> DeletePreset
deletePreset pName_ = DeletePreset' {_dpName = pName_}


-- | The name of the preset to be deleted.
dpName :: Lens' DeletePreset Text
dpName = lens _dpName (\ s a -> s{_dpName = a})

instance AWSRequest DeletePreset where
        type Rs DeletePreset = DeletePresetResponse
        request = delete mediaConvert
        response
          = receiveEmpty
              (\ s h x ->
                 DeletePresetResponse' <$> (pure (fromEnum s)))

instance Hashable DeletePreset where

instance NFData DeletePreset where

instance ToHeaders DeletePreset where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeletePreset where
        toPath DeletePreset'{..}
          = mconcat ["/2017-08-29/presets/", toBS _dpName]

instance ToQuery DeletePreset where
        toQuery = const mempty

-- | /See:/ 'deletePresetResponse' smart constructor.
newtype DeletePresetResponse = DeletePresetResponse'
  { _dprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePresetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dprsResponseStatus' - -- | The response status code.
deletePresetResponse
    :: Int -- ^ 'dprsResponseStatus'
    -> DeletePresetResponse
deletePresetResponse pResponseStatus_ =
  DeletePresetResponse' {_dprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dprsResponseStatus :: Lens' DeletePresetResponse Int
dprsResponseStatus = lens _dprsResponseStatus (\ s a -> s{_dprsResponseStatus = a})

instance NFData DeletePresetResponse where

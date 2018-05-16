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
-- Module      : Network.AWS.ElasticTranscoder.DeletePreset
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The DeletePreset operation removes a preset that you've added in an AWS region.
--
--
module Network.AWS.ElasticTranscoder.DeletePreset
    (
    -- * Creating a Request
      deletePreset
    , DeletePreset
    -- * Request Lenses
    , dpId

    -- * Destructuring the Response
    , deletePresetResponse
    , DeletePresetResponse
    -- * Response Lenses
    , dprsResponseStatus
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.ElasticTranscoder.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The @DeletePresetRequest@ structure.
--
--
--
-- /See:/ 'deletePreset' smart constructor.
newtype DeletePreset = DeletePreset'
  { _dpId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePreset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpId' - The identifier of the preset for which you want to get detailed information.
deletePreset
    :: Text -- ^ 'dpId'
    -> DeletePreset
deletePreset pId_ = DeletePreset' {_dpId = pId_}


-- | The identifier of the preset for which you want to get detailed information.
dpId :: Lens' DeletePreset Text
dpId = lens _dpId (\ s a -> s{_dpId = a})

instance AWSRequest DeletePreset where
        type Rs DeletePreset = DeletePresetResponse
        request = delete elasticTranscoder
        response
          = receiveEmpty
              (\ s h x ->
                 DeletePresetResponse' <$> (pure (fromEnum s)))

instance Hashable DeletePreset where

instance NFData DeletePreset where

instance ToHeaders DeletePreset where
        toHeaders = const mempty

instance ToPath DeletePreset where
        toPath DeletePreset'{..}
          = mconcat ["/2012-09-25/presets/", toBS _dpId]

instance ToQuery DeletePreset where
        toQuery = const mempty

-- | The @DeletePresetResponse@ structure.
--
--
--
-- /See:/ 'deletePresetResponse' smart constructor.
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

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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The DeletePreset operation removes a preset that you\'ve added in an AWS
-- region.
--
-- You can\'t delete the default presets that are included with Elastic
-- Transcoder.
--
-- /See:/ <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/DeletePreset.html AWS API Reference> for DeletePreset.
module Network.AWS.ElasticTranscoder.DeletePreset
    (
    -- * Creating a Request
      DeletePreset
    , deletePreset
    -- * Request Lenses
    , dpId

    -- * Destructuring the Response
    , DeletePresetResponse
    , deletePresetResponse
    -- * Response Lenses
    , dprsStatus
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.ElasticTranscoder.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The @DeletePresetRequest@ structure.
--
-- /See:/ 'deletePreset' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpId'
newtype DeletePreset = DeletePreset'
    { _dpId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeletePreset' smart constructor.
deletePreset :: Text -> DeletePreset
deletePreset pId_ = 
    DeletePreset'
    { _dpId = pId_
    }

-- | The identifier of the preset for which you want to get detailed
-- information.
dpId :: Lens' DeletePreset Text
dpId = lens _dpId (\ s a -> s{_dpId = a});

instance AWSRequest DeletePreset where
        type Sv DeletePreset = ElasticTranscoder
        type Rs DeletePreset = DeletePresetResponse
        request = delete
        response
          = receiveEmpty
              (\ s h x ->
                 DeletePresetResponse' <$> (pure (fromEnum s)))

instance ToHeaders DeletePreset where
        toHeaders = const mempty

instance ToPath DeletePreset where
        toPath DeletePreset'{..}
          = mconcat ["/2012-09-25/presets/", toBS _dpId]

instance ToQuery DeletePreset where
        toQuery = const mempty

-- | The @DeletePresetResponse@ structure.
--
-- /See:/ 'deletePresetResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dprsStatus'
newtype DeletePresetResponse = DeletePresetResponse'
    { _dprsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeletePresetResponse' smart constructor.
deletePresetResponse :: Int -> DeletePresetResponse
deletePresetResponse pStatus_ = 
    DeletePresetResponse'
    { _dprsStatus = pStatus_
    }

-- | Undocumented member.
dprsStatus :: Lens' DeletePresetResponse Int
dprsStatus = lens _dprsStatus (\ s a -> s{_dprsStatus = a});

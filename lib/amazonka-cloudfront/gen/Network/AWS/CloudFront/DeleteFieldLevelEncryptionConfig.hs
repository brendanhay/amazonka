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
-- Module      : Network.AWS.CloudFront.DeleteFieldLevelEncryptionConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove a field-level encryption configuration.
--
--
module Network.AWS.CloudFront.DeleteFieldLevelEncryptionConfig
    (
    -- * Creating a Request
      deleteFieldLevelEncryptionConfig
    , DeleteFieldLevelEncryptionConfig
    -- * Request Lenses
    , dflecIfMatch
    , dflecId

    -- * Destructuring the Response
    , deleteFieldLevelEncryptionConfigResponse
    , DeleteFieldLevelEncryptionConfigResponse
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteFieldLevelEncryptionConfig' smart constructor.
data DeleteFieldLevelEncryptionConfig = DeleteFieldLevelEncryptionConfig'
  { _dflecIfMatch :: !(Maybe Text)
  , _dflecId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFieldLevelEncryptionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dflecIfMatch' - The value of the @ETag@ header that you received when retrieving the configuration identity to delete. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'dflecId' - The ID of the configuration you want to delete from CloudFront.
deleteFieldLevelEncryptionConfig
    :: Text -- ^ 'dflecId'
    -> DeleteFieldLevelEncryptionConfig
deleteFieldLevelEncryptionConfig pId_ =
  DeleteFieldLevelEncryptionConfig' {_dflecIfMatch = Nothing, _dflecId = pId_}


-- | The value of the @ETag@ header that you received when retrieving the configuration identity to delete. For example: @E2QWRUHAPOMQZL@ .
dflecIfMatch :: Lens' DeleteFieldLevelEncryptionConfig (Maybe Text)
dflecIfMatch = lens _dflecIfMatch (\ s a -> s{_dflecIfMatch = a})

-- | The ID of the configuration you want to delete from CloudFront.
dflecId :: Lens' DeleteFieldLevelEncryptionConfig Text
dflecId = lens _dflecId (\ s a -> s{_dflecId = a})

instance AWSRequest DeleteFieldLevelEncryptionConfig
         where
        type Rs DeleteFieldLevelEncryptionConfig =
             DeleteFieldLevelEncryptionConfigResponse
        request = delete cloudFront
        response
          = receiveNull
              DeleteFieldLevelEncryptionConfigResponse'

instance Hashable DeleteFieldLevelEncryptionConfig
         where

instance NFData DeleteFieldLevelEncryptionConfig
         where

instance ToHeaders DeleteFieldLevelEncryptionConfig
         where
        toHeaders DeleteFieldLevelEncryptionConfig'{..}
          = mconcat ["If-Match" =# _dflecIfMatch]

instance ToPath DeleteFieldLevelEncryptionConfig
         where
        toPath DeleteFieldLevelEncryptionConfig'{..}
          = mconcat
              ["/2017-10-30/field-level-encryption/",
               toBS _dflecId]

instance ToQuery DeleteFieldLevelEncryptionConfig
         where
        toQuery = const mempty

-- | /See:/ 'deleteFieldLevelEncryptionConfigResponse' smart constructor.
data DeleteFieldLevelEncryptionConfigResponse =
  DeleteFieldLevelEncryptionConfigResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFieldLevelEncryptionConfigResponse' with the minimum fields required to make a request.
--
deleteFieldLevelEncryptionConfigResponse
    :: DeleteFieldLevelEncryptionConfigResponse
deleteFieldLevelEncryptionConfigResponse =
  DeleteFieldLevelEncryptionConfigResponse'


instance NFData
           DeleteFieldLevelEncryptionConfigResponse
         where

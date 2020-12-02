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
-- Module      : Network.AWS.CloudFront.GetFieldLevelEncryption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the field-level encryption configuration information.
--
--
module Network.AWS.CloudFront.GetFieldLevelEncryption
    (
    -- * Creating a Request
      getFieldLevelEncryption
    , GetFieldLevelEncryption
    -- * Request Lenses
    , gfleId

    -- * Destructuring the Response
    , getFieldLevelEncryptionResponse
    , GetFieldLevelEncryptionResponse
    -- * Response Lenses
    , gflersETag
    , gflersFieldLevelEncryption
    , gflersResponseStatus
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getFieldLevelEncryption' smart constructor.
newtype GetFieldLevelEncryption = GetFieldLevelEncryption'
  { _gfleId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFieldLevelEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfleId' - Request the ID for the field-level encryption configuration information.
getFieldLevelEncryption
    :: Text -- ^ 'gfleId'
    -> GetFieldLevelEncryption
getFieldLevelEncryption pId_ = GetFieldLevelEncryption' {_gfleId = pId_}


-- | Request the ID for the field-level encryption configuration information.
gfleId :: Lens' GetFieldLevelEncryption Text
gfleId = lens _gfleId (\ s a -> s{_gfleId = a})

instance AWSRequest GetFieldLevelEncryption where
        type Rs GetFieldLevelEncryption =
             GetFieldLevelEncryptionResponse
        request = get cloudFront
        response
          = receiveXML
              (\ s h x ->
                 GetFieldLevelEncryptionResponse' <$>
                   (h .#? "ETag") <*> (parseXML x) <*>
                     (pure (fromEnum s)))

instance Hashable GetFieldLevelEncryption where

instance NFData GetFieldLevelEncryption where

instance ToHeaders GetFieldLevelEncryption where
        toHeaders = const mempty

instance ToPath GetFieldLevelEncryption where
        toPath GetFieldLevelEncryption'{..}
          = mconcat
              ["/2017-10-30/field-level-encryption/", toBS _gfleId]

instance ToQuery GetFieldLevelEncryption where
        toQuery = const mempty

-- | /See:/ 'getFieldLevelEncryptionResponse' smart constructor.
data GetFieldLevelEncryptionResponse = GetFieldLevelEncryptionResponse'
  { _gflersETag                 :: !(Maybe Text)
  , _gflersFieldLevelEncryption :: !(Maybe FieldLevelEncryption)
  , _gflersResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFieldLevelEncryptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gflersETag' - The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'gflersFieldLevelEncryption' - Return the field-level encryption configuration information.
--
-- * 'gflersResponseStatus' - -- | The response status code.
getFieldLevelEncryptionResponse
    :: Int -- ^ 'gflersResponseStatus'
    -> GetFieldLevelEncryptionResponse
getFieldLevelEncryptionResponse pResponseStatus_ =
  GetFieldLevelEncryptionResponse'
    { _gflersETag = Nothing
    , _gflersFieldLevelEncryption = Nothing
    , _gflersResponseStatus = pResponseStatus_
    }


-- | The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
gflersETag :: Lens' GetFieldLevelEncryptionResponse (Maybe Text)
gflersETag = lens _gflersETag (\ s a -> s{_gflersETag = a})

-- | Return the field-level encryption configuration information.
gflersFieldLevelEncryption :: Lens' GetFieldLevelEncryptionResponse (Maybe FieldLevelEncryption)
gflersFieldLevelEncryption = lens _gflersFieldLevelEncryption (\ s a -> s{_gflersFieldLevelEncryption = a})

-- | -- | The response status code.
gflersResponseStatus :: Lens' GetFieldLevelEncryptionResponse Int
gflersResponseStatus = lens _gflersResponseStatus (\ s a -> s{_gflersResponseStatus = a})

instance NFData GetFieldLevelEncryptionResponse where

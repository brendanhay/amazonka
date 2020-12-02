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
-- Module      : Network.AWS.CloudFront.GetFieldLevelEncryptionProfileConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the field-level encryption profile configuration information.
--
--
module Network.AWS.CloudFront.GetFieldLevelEncryptionProfileConfig
    (
    -- * Creating a Request
      getFieldLevelEncryptionProfileConfig
    , GetFieldLevelEncryptionProfileConfig
    -- * Request Lenses
    , gflepcId

    -- * Destructuring the Response
    , getFieldLevelEncryptionProfileConfigResponse
    , GetFieldLevelEncryptionProfileConfigResponse
    -- * Response Lenses
    , gflepcrsETag
    , gflepcrsFieldLevelEncryptionProfileConfig
    , gflepcrsResponseStatus
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getFieldLevelEncryptionProfileConfig' smart constructor.
newtype GetFieldLevelEncryptionProfileConfig = GetFieldLevelEncryptionProfileConfig'
  { _gflepcId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFieldLevelEncryptionProfileConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gflepcId' - Get the ID for the field-level encryption profile configuration information.
getFieldLevelEncryptionProfileConfig
    :: Text -- ^ 'gflepcId'
    -> GetFieldLevelEncryptionProfileConfig
getFieldLevelEncryptionProfileConfig pId_ =
  GetFieldLevelEncryptionProfileConfig' {_gflepcId = pId_}


-- | Get the ID for the field-level encryption profile configuration information.
gflepcId :: Lens' GetFieldLevelEncryptionProfileConfig Text
gflepcId = lens _gflepcId (\ s a -> s{_gflepcId = a})

instance AWSRequest
           GetFieldLevelEncryptionProfileConfig
         where
        type Rs GetFieldLevelEncryptionProfileConfig =
             GetFieldLevelEncryptionProfileConfigResponse
        request = get cloudFront
        response
          = receiveXML
              (\ s h x ->
                 GetFieldLevelEncryptionProfileConfigResponse' <$>
                   (h .#? "ETag") <*> (parseXML x) <*>
                     (pure (fromEnum s)))

instance Hashable
           GetFieldLevelEncryptionProfileConfig
         where

instance NFData GetFieldLevelEncryptionProfileConfig
         where

instance ToHeaders
           GetFieldLevelEncryptionProfileConfig
         where
        toHeaders = const mempty

instance ToPath GetFieldLevelEncryptionProfileConfig
         where
        toPath GetFieldLevelEncryptionProfileConfig'{..}
          = mconcat
              ["/2017-10-30/field-level-encryption-profile/",
               toBS _gflepcId, "/config"]

instance ToQuery GetFieldLevelEncryptionProfileConfig
         where
        toQuery = const mempty

-- | /See:/ 'getFieldLevelEncryptionProfileConfigResponse' smart constructor.
data GetFieldLevelEncryptionProfileConfigResponse = GetFieldLevelEncryptionProfileConfigResponse'
  { _gflepcrsETag :: !(Maybe Text)
  , _gflepcrsFieldLevelEncryptionProfileConfig :: !(Maybe FieldLevelEncryptionProfileConfig)
  , _gflepcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFieldLevelEncryptionProfileConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gflepcrsETag' - The current version of the field-level encryption profile configuration result. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'gflepcrsFieldLevelEncryptionProfileConfig' - Return the field-level encryption profile configuration information.
--
-- * 'gflepcrsResponseStatus' - -- | The response status code.
getFieldLevelEncryptionProfileConfigResponse
    :: Int -- ^ 'gflepcrsResponseStatus'
    -> GetFieldLevelEncryptionProfileConfigResponse
getFieldLevelEncryptionProfileConfigResponse pResponseStatus_ =
  GetFieldLevelEncryptionProfileConfigResponse'
    { _gflepcrsETag = Nothing
    , _gflepcrsFieldLevelEncryptionProfileConfig = Nothing
    , _gflepcrsResponseStatus = pResponseStatus_
    }


-- | The current version of the field-level encryption profile configuration result. For example: @E2QWRUHAPOMQZL@ .
gflepcrsETag :: Lens' GetFieldLevelEncryptionProfileConfigResponse (Maybe Text)
gflepcrsETag = lens _gflepcrsETag (\ s a -> s{_gflepcrsETag = a})

-- | Return the field-level encryption profile configuration information.
gflepcrsFieldLevelEncryptionProfileConfig :: Lens' GetFieldLevelEncryptionProfileConfigResponse (Maybe FieldLevelEncryptionProfileConfig)
gflepcrsFieldLevelEncryptionProfileConfig = lens _gflepcrsFieldLevelEncryptionProfileConfig (\ s a -> s{_gflepcrsFieldLevelEncryptionProfileConfig = a})

-- | -- | The response status code.
gflepcrsResponseStatus :: Lens' GetFieldLevelEncryptionProfileConfigResponse Int
gflepcrsResponseStatus = lens _gflepcrsResponseStatus (\ s a -> s{_gflepcrsResponseStatus = a})

instance NFData
           GetFieldLevelEncryptionProfileConfigResponse
         where

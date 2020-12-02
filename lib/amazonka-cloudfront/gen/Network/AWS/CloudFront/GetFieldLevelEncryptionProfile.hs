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
-- Module      : Network.AWS.CloudFront.GetFieldLevelEncryptionProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the field-level encryption profile information.
--
--
module Network.AWS.CloudFront.GetFieldLevelEncryptionProfile
    (
    -- * Creating a Request
      getFieldLevelEncryptionProfile
    , GetFieldLevelEncryptionProfile
    -- * Request Lenses
    , gflepId

    -- * Destructuring the Response
    , getFieldLevelEncryptionProfileResponse
    , GetFieldLevelEncryptionProfileResponse
    -- * Response Lenses
    , gfleprsETag
    , gfleprsFieldLevelEncryptionProfile
    , gfleprsResponseStatus
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getFieldLevelEncryptionProfile' smart constructor.
newtype GetFieldLevelEncryptionProfile = GetFieldLevelEncryptionProfile'
  { _gflepId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFieldLevelEncryptionProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gflepId' - Get the ID for the field-level encryption profile information.
getFieldLevelEncryptionProfile
    :: Text -- ^ 'gflepId'
    -> GetFieldLevelEncryptionProfile
getFieldLevelEncryptionProfile pId_ =
  GetFieldLevelEncryptionProfile' {_gflepId = pId_}


-- | Get the ID for the field-level encryption profile information.
gflepId :: Lens' GetFieldLevelEncryptionProfile Text
gflepId = lens _gflepId (\ s a -> s{_gflepId = a})

instance AWSRequest GetFieldLevelEncryptionProfile
         where
        type Rs GetFieldLevelEncryptionProfile =
             GetFieldLevelEncryptionProfileResponse
        request = get cloudFront
        response
          = receiveXML
              (\ s h x ->
                 GetFieldLevelEncryptionProfileResponse' <$>
                   (h .#? "ETag") <*> (parseXML x) <*>
                     (pure (fromEnum s)))

instance Hashable GetFieldLevelEncryptionProfile
         where

instance NFData GetFieldLevelEncryptionProfile where

instance ToHeaders GetFieldLevelEncryptionProfile
         where
        toHeaders = const mempty

instance ToPath GetFieldLevelEncryptionProfile where
        toPath GetFieldLevelEncryptionProfile'{..}
          = mconcat
              ["/2017-10-30/field-level-encryption-profile/",
               toBS _gflepId]

instance ToQuery GetFieldLevelEncryptionProfile where
        toQuery = const mempty

-- | /See:/ 'getFieldLevelEncryptionProfileResponse' smart constructor.
data GetFieldLevelEncryptionProfileResponse = GetFieldLevelEncryptionProfileResponse'
  { _gfleprsETag                        :: !(Maybe Text)
  , _gfleprsFieldLevelEncryptionProfile :: !(Maybe FieldLevelEncryptionProfile)
  , _gfleprsResponseStatus              :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFieldLevelEncryptionProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfleprsETag' - The current version of the field level encryption profile. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'gfleprsFieldLevelEncryptionProfile' - Return the field-level encryption profile information.
--
-- * 'gfleprsResponseStatus' - -- | The response status code.
getFieldLevelEncryptionProfileResponse
    :: Int -- ^ 'gfleprsResponseStatus'
    -> GetFieldLevelEncryptionProfileResponse
getFieldLevelEncryptionProfileResponse pResponseStatus_ =
  GetFieldLevelEncryptionProfileResponse'
    { _gfleprsETag = Nothing
    , _gfleprsFieldLevelEncryptionProfile = Nothing
    , _gfleprsResponseStatus = pResponseStatus_
    }


-- | The current version of the field level encryption profile. For example: @E2QWRUHAPOMQZL@ .
gfleprsETag :: Lens' GetFieldLevelEncryptionProfileResponse (Maybe Text)
gfleprsETag = lens _gfleprsETag (\ s a -> s{_gfleprsETag = a})

-- | Return the field-level encryption profile information.
gfleprsFieldLevelEncryptionProfile :: Lens' GetFieldLevelEncryptionProfileResponse (Maybe FieldLevelEncryptionProfile)
gfleprsFieldLevelEncryptionProfile = lens _gfleprsFieldLevelEncryptionProfile (\ s a -> s{_gfleprsFieldLevelEncryptionProfile = a})

-- | -- | The response status code.
gfleprsResponseStatus :: Lens' GetFieldLevelEncryptionProfileResponse Int
gfleprsResponseStatus = lens _gfleprsResponseStatus (\ s a -> s{_gfleprsResponseStatus = a})

instance NFData
           GetFieldLevelEncryptionProfileResponse
         where

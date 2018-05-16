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
-- Module      : Network.AWS.CloudFront.DeleteFieldLevelEncryptionProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove a field-level encryption profile.
--
--
module Network.AWS.CloudFront.DeleteFieldLevelEncryptionProfile
    (
    -- * Creating a Request
      deleteFieldLevelEncryptionProfile
    , DeleteFieldLevelEncryptionProfile
    -- * Request Lenses
    , dflepIfMatch
    , dflepId

    -- * Destructuring the Response
    , deleteFieldLevelEncryptionProfileResponse
    , DeleteFieldLevelEncryptionProfileResponse
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteFieldLevelEncryptionProfile' smart constructor.
data DeleteFieldLevelEncryptionProfile = DeleteFieldLevelEncryptionProfile'
  { _dflepIfMatch :: !(Maybe Text)
  , _dflepId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFieldLevelEncryptionProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dflepIfMatch' - The value of the @ETag@ header that you received when retrieving the profile to delete. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'dflepId' - Request the ID of the profile you want to delete from CloudFront.
deleteFieldLevelEncryptionProfile
    :: Text -- ^ 'dflepId'
    -> DeleteFieldLevelEncryptionProfile
deleteFieldLevelEncryptionProfile pId_ =
  DeleteFieldLevelEncryptionProfile' {_dflepIfMatch = Nothing, _dflepId = pId_}


-- | The value of the @ETag@ header that you received when retrieving the profile to delete. For example: @E2QWRUHAPOMQZL@ .
dflepIfMatch :: Lens' DeleteFieldLevelEncryptionProfile (Maybe Text)
dflepIfMatch = lens _dflepIfMatch (\ s a -> s{_dflepIfMatch = a})

-- | Request the ID of the profile you want to delete from CloudFront.
dflepId :: Lens' DeleteFieldLevelEncryptionProfile Text
dflepId = lens _dflepId (\ s a -> s{_dflepId = a})

instance AWSRequest DeleteFieldLevelEncryptionProfile
         where
        type Rs DeleteFieldLevelEncryptionProfile =
             DeleteFieldLevelEncryptionProfileResponse
        request = delete cloudFront
        response
          = receiveNull
              DeleteFieldLevelEncryptionProfileResponse'

instance Hashable DeleteFieldLevelEncryptionProfile
         where

instance NFData DeleteFieldLevelEncryptionProfile
         where

instance ToHeaders DeleteFieldLevelEncryptionProfile
         where
        toHeaders DeleteFieldLevelEncryptionProfile'{..}
          = mconcat ["If-Match" =# _dflepIfMatch]

instance ToPath DeleteFieldLevelEncryptionProfile
         where
        toPath DeleteFieldLevelEncryptionProfile'{..}
          = mconcat
              ["/2017-10-30/field-level-encryption-profile/",
               toBS _dflepId]

instance ToQuery DeleteFieldLevelEncryptionProfile
         where
        toQuery = const mempty

-- | /See:/ 'deleteFieldLevelEncryptionProfileResponse' smart constructor.
data DeleteFieldLevelEncryptionProfileResponse =
  DeleteFieldLevelEncryptionProfileResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFieldLevelEncryptionProfileResponse' with the minimum fields required to make a request.
--
deleteFieldLevelEncryptionProfileResponse
    :: DeleteFieldLevelEncryptionProfileResponse
deleteFieldLevelEncryptionProfileResponse =
  DeleteFieldLevelEncryptionProfileResponse'


instance NFData
           DeleteFieldLevelEncryptionProfileResponse
         where

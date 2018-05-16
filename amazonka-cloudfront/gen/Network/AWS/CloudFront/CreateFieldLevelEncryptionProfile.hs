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
-- Module      : Network.AWS.CloudFront.CreateFieldLevelEncryptionProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a field-level encryption profile.
--
--
module Network.AWS.CloudFront.CreateFieldLevelEncryptionProfile
    (
    -- * Creating a Request
      createFieldLevelEncryptionProfile
    , CreateFieldLevelEncryptionProfile
    -- * Request Lenses
    , cflepFieldLevelEncryptionProfileConfig

    -- * Destructuring the Response
    , createFieldLevelEncryptionProfileResponse
    , CreateFieldLevelEncryptionProfileResponse
    -- * Response Lenses
    , cfleprsETag
    , cfleprsLocation
    , cfleprsFieldLevelEncryptionProfile
    , cfleprsResponseStatus
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createFieldLevelEncryptionProfile' smart constructor.
newtype CreateFieldLevelEncryptionProfile = CreateFieldLevelEncryptionProfile'
  { _cflepFieldLevelEncryptionProfileConfig :: FieldLevelEncryptionProfileConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFieldLevelEncryptionProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cflepFieldLevelEncryptionProfileConfig' - The request to create a field-level encryption profile.
createFieldLevelEncryptionProfile
    :: FieldLevelEncryptionProfileConfig -- ^ 'cflepFieldLevelEncryptionProfileConfig'
    -> CreateFieldLevelEncryptionProfile
createFieldLevelEncryptionProfile pFieldLevelEncryptionProfileConfig_ =
  CreateFieldLevelEncryptionProfile'
    { _cflepFieldLevelEncryptionProfileConfig =
        pFieldLevelEncryptionProfileConfig_
    }


-- | The request to create a field-level encryption profile.
cflepFieldLevelEncryptionProfileConfig :: Lens' CreateFieldLevelEncryptionProfile FieldLevelEncryptionProfileConfig
cflepFieldLevelEncryptionProfileConfig = lens _cflepFieldLevelEncryptionProfileConfig (\ s a -> s{_cflepFieldLevelEncryptionProfileConfig = a})

instance AWSRequest CreateFieldLevelEncryptionProfile
         where
        type Rs CreateFieldLevelEncryptionProfile =
             CreateFieldLevelEncryptionProfileResponse
        request = postXML cloudFront
        response
          = receiveXML
              (\ s h x ->
                 CreateFieldLevelEncryptionProfileResponse' <$>
                   (h .#? "ETag") <*> (h .#? "Location") <*>
                     (parseXML x)
                     <*> (pure (fromEnum s)))

instance Hashable CreateFieldLevelEncryptionProfile
         where

instance NFData CreateFieldLevelEncryptionProfile
         where

instance ToElement CreateFieldLevelEncryptionProfile
         where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2017-10-30/}FieldLevelEncryptionProfileConfig"
              .
              _cflepFieldLevelEncryptionProfileConfig

instance ToHeaders CreateFieldLevelEncryptionProfile
         where
        toHeaders = const mempty

instance ToPath CreateFieldLevelEncryptionProfile
         where
        toPath
          = const "/2017-10-30/field-level-encryption-profile"

instance ToQuery CreateFieldLevelEncryptionProfile
         where
        toQuery = const mempty

-- | /See:/ 'createFieldLevelEncryptionProfileResponse' smart constructor.
data CreateFieldLevelEncryptionProfileResponse = CreateFieldLevelEncryptionProfileResponse'
  { _cfleprsETag                        :: !(Maybe Text)
  , _cfleprsLocation                    :: !(Maybe Text)
  , _cfleprsFieldLevelEncryptionProfile :: !(Maybe FieldLevelEncryptionProfile)
  , _cfleprsResponseStatus              :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFieldLevelEncryptionProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfleprsETag' - The current version of the field level encryption profile. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'cfleprsLocation' - The fully qualified URI of the new profile resource just created. For example: @https://cloudfront.amazonaws.com/2010-11-01/field-level-encryption-profile/EDFDVBD632BHDS5@ .
--
-- * 'cfleprsFieldLevelEncryptionProfile' - Returned when you create a new field-level encryption profile.
--
-- * 'cfleprsResponseStatus' - -- | The response status code.
createFieldLevelEncryptionProfileResponse
    :: Int -- ^ 'cfleprsResponseStatus'
    -> CreateFieldLevelEncryptionProfileResponse
createFieldLevelEncryptionProfileResponse pResponseStatus_ =
  CreateFieldLevelEncryptionProfileResponse'
    { _cfleprsETag = Nothing
    , _cfleprsLocation = Nothing
    , _cfleprsFieldLevelEncryptionProfile = Nothing
    , _cfleprsResponseStatus = pResponseStatus_
    }


-- | The current version of the field level encryption profile. For example: @E2QWRUHAPOMQZL@ .
cfleprsETag :: Lens' CreateFieldLevelEncryptionProfileResponse (Maybe Text)
cfleprsETag = lens _cfleprsETag (\ s a -> s{_cfleprsETag = a})

-- | The fully qualified URI of the new profile resource just created. For example: @https://cloudfront.amazonaws.com/2010-11-01/field-level-encryption-profile/EDFDVBD632BHDS5@ .
cfleprsLocation :: Lens' CreateFieldLevelEncryptionProfileResponse (Maybe Text)
cfleprsLocation = lens _cfleprsLocation (\ s a -> s{_cfleprsLocation = a})

-- | Returned when you create a new field-level encryption profile.
cfleprsFieldLevelEncryptionProfile :: Lens' CreateFieldLevelEncryptionProfileResponse (Maybe FieldLevelEncryptionProfile)
cfleprsFieldLevelEncryptionProfile = lens _cfleprsFieldLevelEncryptionProfile (\ s a -> s{_cfleprsFieldLevelEncryptionProfile = a})

-- | -- | The response status code.
cfleprsResponseStatus :: Lens' CreateFieldLevelEncryptionProfileResponse Int
cfleprsResponseStatus = lens _cfleprsResponseStatus (\ s a -> s{_cfleprsResponseStatus = a})

instance NFData
           CreateFieldLevelEncryptionProfileResponse
         where

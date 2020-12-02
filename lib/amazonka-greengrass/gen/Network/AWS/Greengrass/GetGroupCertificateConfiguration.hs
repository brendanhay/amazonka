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
-- Module      : Network.AWS.Greengrass.GetGroupCertificateConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current configuration for the CA used by the group.
module Network.AWS.Greengrass.GetGroupCertificateConfiguration
    (
    -- * Creating a Request
      getGroupCertificateConfiguration
    , GetGroupCertificateConfiguration
    -- * Request Lenses
    , ggccGroupId

    -- * Destructuring the Response
    , getGroupCertificateConfigurationResponse
    , GetGroupCertificateConfigurationResponse
    -- * Response Lenses
    , ggccrsCertificateAuthorityExpiryInMilliseconds
    , ggccrsGroupId
    , ggccrsCertificateExpiryInMilliseconds
    , ggccrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getGroupCertificateConfiguration' smart constructor.
newtype GetGroupCertificateConfiguration = GetGroupCertificateConfiguration'
  { _ggccGroupId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetGroupCertificateConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggccGroupId' - The ID of the AWS Greengrass group.
getGroupCertificateConfiguration
    :: Text -- ^ 'ggccGroupId'
    -> GetGroupCertificateConfiguration
getGroupCertificateConfiguration pGroupId_ =
  GetGroupCertificateConfiguration' {_ggccGroupId = pGroupId_}


-- | The ID of the AWS Greengrass group.
ggccGroupId :: Lens' GetGroupCertificateConfiguration Text
ggccGroupId = lens _ggccGroupId (\ s a -> s{_ggccGroupId = a})

instance AWSRequest GetGroupCertificateConfiguration
         where
        type Rs GetGroupCertificateConfiguration =
             GetGroupCertificateConfigurationResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 GetGroupCertificateConfigurationResponse' <$>
                   (x .?> "CertificateAuthorityExpiryInMilliseconds")
                     <*> (x .?> "GroupId")
                     <*> (x .?> "CertificateExpiryInMilliseconds")
                     <*> (pure (fromEnum s)))

instance Hashable GetGroupCertificateConfiguration
         where

instance NFData GetGroupCertificateConfiguration
         where

instance ToHeaders GetGroupCertificateConfiguration
         where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetGroupCertificateConfiguration
         where
        toPath GetGroupCertificateConfiguration'{..}
          = mconcat
              ["/greengrass/groups/", toBS _ggccGroupId,
               "/certificateauthorities/configuration/expiry"]

instance ToQuery GetGroupCertificateConfiguration
         where
        toQuery = const mempty

-- | /See:/ 'getGroupCertificateConfigurationResponse' smart constructor.
data GetGroupCertificateConfigurationResponse = GetGroupCertificateConfigurationResponse'
  { _ggccrsCertificateAuthorityExpiryInMilliseconds :: !(Maybe Text)
  , _ggccrsGroupId                                  :: !(Maybe Text)
  , _ggccrsCertificateExpiryInMilliseconds          :: !(Maybe Text)
  , _ggccrsResponseStatus                           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetGroupCertificateConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggccrsCertificateAuthorityExpiryInMilliseconds' - The amount of time remaining before the certificate authority expires, in milliseconds.
--
-- * 'ggccrsGroupId' - The ID of the group certificate configuration.
--
-- * 'ggccrsCertificateExpiryInMilliseconds' - The amount of time remaining before the certificate expires, in milliseconds.
--
-- * 'ggccrsResponseStatus' - -- | The response status code.
getGroupCertificateConfigurationResponse
    :: Int -- ^ 'ggccrsResponseStatus'
    -> GetGroupCertificateConfigurationResponse
getGroupCertificateConfigurationResponse pResponseStatus_ =
  GetGroupCertificateConfigurationResponse'
    { _ggccrsCertificateAuthorityExpiryInMilliseconds = Nothing
    , _ggccrsGroupId = Nothing
    , _ggccrsCertificateExpiryInMilliseconds = Nothing
    , _ggccrsResponseStatus = pResponseStatus_
    }


-- | The amount of time remaining before the certificate authority expires, in milliseconds.
ggccrsCertificateAuthorityExpiryInMilliseconds :: Lens' GetGroupCertificateConfigurationResponse (Maybe Text)
ggccrsCertificateAuthorityExpiryInMilliseconds = lens _ggccrsCertificateAuthorityExpiryInMilliseconds (\ s a -> s{_ggccrsCertificateAuthorityExpiryInMilliseconds = a})

-- | The ID of the group certificate configuration.
ggccrsGroupId :: Lens' GetGroupCertificateConfigurationResponse (Maybe Text)
ggccrsGroupId = lens _ggccrsGroupId (\ s a -> s{_ggccrsGroupId = a})

-- | The amount of time remaining before the certificate expires, in milliseconds.
ggccrsCertificateExpiryInMilliseconds :: Lens' GetGroupCertificateConfigurationResponse (Maybe Text)
ggccrsCertificateExpiryInMilliseconds = lens _ggccrsCertificateExpiryInMilliseconds (\ s a -> s{_ggccrsCertificateExpiryInMilliseconds = a})

-- | -- | The response status code.
ggccrsResponseStatus :: Lens' GetGroupCertificateConfigurationResponse Int
ggccrsResponseStatus = lens _ggccrsResponseStatus (\ s a -> s{_ggccrsResponseStatus = a})

instance NFData
           GetGroupCertificateConfigurationResponse
         where

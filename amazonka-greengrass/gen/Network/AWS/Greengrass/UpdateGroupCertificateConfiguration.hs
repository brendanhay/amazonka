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
-- Module      : Network.AWS.Greengrass.UpdateGroupCertificateConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Certificate expiry time for a group.
module Network.AWS.Greengrass.UpdateGroupCertificateConfiguration
    (
    -- * Creating a Request
      updateGroupCertificateConfiguration
    , UpdateGroupCertificateConfiguration
    -- * Request Lenses
    , ugccCertificateExpiryInMilliseconds
    , ugccGroupId

    -- * Destructuring the Response
    , updateGroupCertificateConfigurationResponse
    , UpdateGroupCertificateConfigurationResponse
    -- * Response Lenses
    , ugccrsCertificateAuthorityExpiryInMilliseconds
    , ugccrsGroupId
    , ugccrsCertificateExpiryInMilliseconds
    , ugccrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateGroupCertificateConfiguration' smart constructor.
data UpdateGroupCertificateConfiguration = UpdateGroupCertificateConfiguration'
  { _ugccCertificateExpiryInMilliseconds :: !(Maybe Text)
  , _ugccGroupId                         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGroupCertificateConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugccCertificateExpiryInMilliseconds' - The amount of time remaining before the certificate expires, in milliseconds.
--
-- * 'ugccGroupId' - The ID of the AWS Greengrass group.
updateGroupCertificateConfiguration
    :: Text -- ^ 'ugccGroupId'
    -> UpdateGroupCertificateConfiguration
updateGroupCertificateConfiguration pGroupId_ =
  UpdateGroupCertificateConfiguration'
    {_ugccCertificateExpiryInMilliseconds = Nothing, _ugccGroupId = pGroupId_}


-- | The amount of time remaining before the certificate expires, in milliseconds.
ugccCertificateExpiryInMilliseconds :: Lens' UpdateGroupCertificateConfiguration (Maybe Text)
ugccCertificateExpiryInMilliseconds = lens _ugccCertificateExpiryInMilliseconds (\ s a -> s{_ugccCertificateExpiryInMilliseconds = a})

-- | The ID of the AWS Greengrass group.
ugccGroupId :: Lens' UpdateGroupCertificateConfiguration Text
ugccGroupId = lens _ugccGroupId (\ s a -> s{_ugccGroupId = a})

instance AWSRequest
           UpdateGroupCertificateConfiguration
         where
        type Rs UpdateGroupCertificateConfiguration =
             UpdateGroupCertificateConfigurationResponse
        request = putJSON greengrass
        response
          = receiveJSON
              (\ s h x ->
                 UpdateGroupCertificateConfigurationResponse' <$>
                   (x .?> "CertificateAuthorityExpiryInMilliseconds")
                     <*> (x .?> "GroupId")
                     <*> (x .?> "CertificateExpiryInMilliseconds")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateGroupCertificateConfiguration
         where

instance NFData UpdateGroupCertificateConfiguration
         where

instance ToHeaders
           UpdateGroupCertificateConfiguration
         where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateGroupCertificateConfiguration
         where
        toJSON UpdateGroupCertificateConfiguration'{..}
          = object
              (catMaybes
                 [("CertificateExpiryInMilliseconds" .=) <$>
                    _ugccCertificateExpiryInMilliseconds])

instance ToPath UpdateGroupCertificateConfiguration
         where
        toPath UpdateGroupCertificateConfiguration'{..}
          = mconcat
              ["/greengrass/groups/", toBS _ugccGroupId,
               "/certificateauthorities/configuration/expiry"]

instance ToQuery UpdateGroupCertificateConfiguration
         where
        toQuery = const mempty

-- | /See:/ 'updateGroupCertificateConfigurationResponse' smart constructor.
data UpdateGroupCertificateConfigurationResponse = UpdateGroupCertificateConfigurationResponse'
  { _ugccrsCertificateAuthorityExpiryInMilliseconds :: !(Maybe Text)
  , _ugccrsGroupId                                  :: !(Maybe Text)
  , _ugccrsCertificateExpiryInMilliseconds          :: !(Maybe Text)
  , _ugccrsResponseStatus                           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGroupCertificateConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugccrsCertificateAuthorityExpiryInMilliseconds' - The amount of time remaining before the certificate authority expires, in milliseconds.
--
-- * 'ugccrsGroupId' - The ID of the group certificate configuration.
--
-- * 'ugccrsCertificateExpiryInMilliseconds' - The amount of time remaining before the certificate expires, in milliseconds.
--
-- * 'ugccrsResponseStatus' - -- | The response status code.
updateGroupCertificateConfigurationResponse
    :: Int -- ^ 'ugccrsResponseStatus'
    -> UpdateGroupCertificateConfigurationResponse
updateGroupCertificateConfigurationResponse pResponseStatus_ =
  UpdateGroupCertificateConfigurationResponse'
    { _ugccrsCertificateAuthorityExpiryInMilliseconds = Nothing
    , _ugccrsGroupId = Nothing
    , _ugccrsCertificateExpiryInMilliseconds = Nothing
    , _ugccrsResponseStatus = pResponseStatus_
    }


-- | The amount of time remaining before the certificate authority expires, in milliseconds.
ugccrsCertificateAuthorityExpiryInMilliseconds :: Lens' UpdateGroupCertificateConfigurationResponse (Maybe Text)
ugccrsCertificateAuthorityExpiryInMilliseconds = lens _ugccrsCertificateAuthorityExpiryInMilliseconds (\ s a -> s{_ugccrsCertificateAuthorityExpiryInMilliseconds = a})

-- | The ID of the group certificate configuration.
ugccrsGroupId :: Lens' UpdateGroupCertificateConfigurationResponse (Maybe Text)
ugccrsGroupId = lens _ugccrsGroupId (\ s a -> s{_ugccrsGroupId = a})

-- | The amount of time remaining before the certificate expires, in milliseconds.
ugccrsCertificateExpiryInMilliseconds :: Lens' UpdateGroupCertificateConfigurationResponse (Maybe Text)
ugccrsCertificateExpiryInMilliseconds = lens _ugccrsCertificateExpiryInMilliseconds (\ s a -> s{_ugccrsCertificateExpiryInMilliseconds = a})

-- | -- | The response status code.
ugccrsResponseStatus :: Lens' UpdateGroupCertificateConfigurationResponse Int
ugccrsResponseStatus = lens _ugccrsResponseStatus (\ s a -> s{_ugccrsResponseStatus = a})

instance NFData
           UpdateGroupCertificateConfigurationResponse
         where

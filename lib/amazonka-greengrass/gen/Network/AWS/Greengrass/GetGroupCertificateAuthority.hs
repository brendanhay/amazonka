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
-- Module      : Network.AWS.Greengrass.GetGroupCertificateAuthority
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retreives the CA associated with a group. Returns the public key of the CA.
module Network.AWS.Greengrass.GetGroupCertificateAuthority
    (
    -- * Creating a Request
      getGroupCertificateAuthority
    , GetGroupCertificateAuthority
    -- * Request Lenses
    , ggcaCertificateAuthorityId
    , ggcaGroupId

    -- * Destructuring the Response
    , getGroupCertificateAuthorityResponse
    , GetGroupCertificateAuthorityResponse
    -- * Response Lenses
    , ggcarsPemEncodedCertificate
    , ggcarsGroupCertificateAuthorityARN
    , ggcarsGroupCertificateAuthorityId
    , ggcarsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getGroupCertificateAuthority' smart constructor.
data GetGroupCertificateAuthority = GetGroupCertificateAuthority'
  { _ggcaCertificateAuthorityId :: !Text
  , _ggcaGroupId                :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetGroupCertificateAuthority' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggcaCertificateAuthorityId' - The ID of the certificate authority.
--
-- * 'ggcaGroupId' - The ID of the AWS Greengrass group.
getGroupCertificateAuthority
    :: Text -- ^ 'ggcaCertificateAuthorityId'
    -> Text -- ^ 'ggcaGroupId'
    -> GetGroupCertificateAuthority
getGroupCertificateAuthority pCertificateAuthorityId_ pGroupId_ =
  GetGroupCertificateAuthority'
    { _ggcaCertificateAuthorityId = pCertificateAuthorityId_
    , _ggcaGroupId = pGroupId_
    }


-- | The ID of the certificate authority.
ggcaCertificateAuthorityId :: Lens' GetGroupCertificateAuthority Text
ggcaCertificateAuthorityId = lens _ggcaCertificateAuthorityId (\ s a -> s{_ggcaCertificateAuthorityId = a})

-- | The ID of the AWS Greengrass group.
ggcaGroupId :: Lens' GetGroupCertificateAuthority Text
ggcaGroupId = lens _ggcaGroupId (\ s a -> s{_ggcaGroupId = a})

instance AWSRequest GetGroupCertificateAuthority
         where
        type Rs GetGroupCertificateAuthority =
             GetGroupCertificateAuthorityResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 GetGroupCertificateAuthorityResponse' <$>
                   (x .?> "PemEncodedCertificate") <*>
                     (x .?> "GroupCertificateAuthorityArn")
                     <*> (x .?> "GroupCertificateAuthorityId")
                     <*> (pure (fromEnum s)))

instance Hashable GetGroupCertificateAuthority where

instance NFData GetGroupCertificateAuthority where

instance ToHeaders GetGroupCertificateAuthority where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetGroupCertificateAuthority where
        toPath GetGroupCertificateAuthority'{..}
          = mconcat
              ["/greengrass/groups/", toBS _ggcaGroupId,
               "/certificateauthorities/",
               toBS _ggcaCertificateAuthorityId]

instance ToQuery GetGroupCertificateAuthority where
        toQuery = const mempty

-- | /See:/ 'getGroupCertificateAuthorityResponse' smart constructor.
data GetGroupCertificateAuthorityResponse = GetGroupCertificateAuthorityResponse'
  { _ggcarsPemEncodedCertificate        :: !(Maybe Text)
  , _ggcarsGroupCertificateAuthorityARN :: !(Maybe Text)
  , _ggcarsGroupCertificateAuthorityId  :: !(Maybe Text)
  , _ggcarsResponseStatus               :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetGroupCertificateAuthorityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggcarsPemEncodedCertificate' - The PEM encoded certificate for the group.
--
-- * 'ggcarsGroupCertificateAuthorityARN' - The ARN of the certificate authority for the group.
--
-- * 'ggcarsGroupCertificateAuthorityId' - The ID of the certificate authority for the group.
--
-- * 'ggcarsResponseStatus' - -- | The response status code.
getGroupCertificateAuthorityResponse
    :: Int -- ^ 'ggcarsResponseStatus'
    -> GetGroupCertificateAuthorityResponse
getGroupCertificateAuthorityResponse pResponseStatus_ =
  GetGroupCertificateAuthorityResponse'
    { _ggcarsPemEncodedCertificate = Nothing
    , _ggcarsGroupCertificateAuthorityARN = Nothing
    , _ggcarsGroupCertificateAuthorityId = Nothing
    , _ggcarsResponseStatus = pResponseStatus_
    }


-- | The PEM encoded certificate for the group.
ggcarsPemEncodedCertificate :: Lens' GetGroupCertificateAuthorityResponse (Maybe Text)
ggcarsPemEncodedCertificate = lens _ggcarsPemEncodedCertificate (\ s a -> s{_ggcarsPemEncodedCertificate = a})

-- | The ARN of the certificate authority for the group.
ggcarsGroupCertificateAuthorityARN :: Lens' GetGroupCertificateAuthorityResponse (Maybe Text)
ggcarsGroupCertificateAuthorityARN = lens _ggcarsGroupCertificateAuthorityARN (\ s a -> s{_ggcarsGroupCertificateAuthorityARN = a})

-- | The ID of the certificate authority for the group.
ggcarsGroupCertificateAuthorityId :: Lens' GetGroupCertificateAuthorityResponse (Maybe Text)
ggcarsGroupCertificateAuthorityId = lens _ggcarsGroupCertificateAuthorityId (\ s a -> s{_ggcarsGroupCertificateAuthorityId = a})

-- | -- | The response status code.
ggcarsResponseStatus :: Lens' GetGroupCertificateAuthorityResponse Int
ggcarsResponseStatus = lens _ggcarsResponseStatus (\ s a -> s{_ggcarsResponseStatus = a})

instance NFData GetGroupCertificateAuthorityResponse
         where

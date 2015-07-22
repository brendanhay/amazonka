{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetSAMLProvider
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the SAML provider metadocument that was uploaded when the
-- provider was created or updated.
--
-- This operation requires
-- <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4>.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetSAMLProvider.html>
module Network.AWS.IAM.GetSAMLProvider
    (
    -- * Request
      GetSAMLProvider
    -- ** Request constructor
    , getSAMLProvider
    -- ** Request lenses
    , gsamlprqSAMLProviderARN

    -- * Response
    , GetSAMLProviderResponse
    -- ** Response constructor
    , getSAMLProviderResponse
    -- ** Response lenses
    , gsamlprsCreateDate
    , gsamlprsValidUntil
    , gsamlprsSAMLMetadataDocument
    , gsamlprsStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getSAMLProvider' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsamlprqSAMLProviderARN'
newtype GetSAMLProvider = GetSAMLProvider'
    { _gsamlprqSAMLProviderARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetSAMLProvider' smart constructor.
getSAMLProvider :: Text -> GetSAMLProvider
getSAMLProvider pSAMLProviderARN =
    GetSAMLProvider'
    { _gsamlprqSAMLProviderARN = pSAMLProviderARN
    }

-- | The Amazon Resource Name (ARN) of the SAML provider to get information
-- about.
gsamlprqSAMLProviderARN :: Lens' GetSAMLProvider Text
gsamlprqSAMLProviderARN = lens _gsamlprqSAMLProviderARN (\ s a -> s{_gsamlprqSAMLProviderARN = a});

instance AWSRequest GetSAMLProvider where
        type Sv GetSAMLProvider = IAM
        type Rs GetSAMLProvider = GetSAMLProviderResponse
        request = post
        response
          = receiveXMLWrapper "GetSAMLProviderResult"
              (\ s h x ->
                 GetSAMLProviderResponse' <$>
                   (x .@? "CreateDate") <*> (x .@? "ValidUntil") <*>
                     (x .@? "SAMLMetadataDocument")
                     <*> (pure (fromEnum s)))

instance ToHeaders GetSAMLProvider where
        toHeaders = const mempty

instance ToPath GetSAMLProvider where
        toPath = const "/"

instance ToQuery GetSAMLProvider where
        toQuery GetSAMLProvider'{..}
          = mconcat
              ["Action" =: ("GetSAMLProvider" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "SAMLProviderArn" =: _gsamlprqSAMLProviderARN]

-- | Contains the response to a successful GetSAMLProvider request.
--
-- /See:/ 'getSAMLProviderResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsamlprsCreateDate'
--
-- * 'gsamlprsValidUntil'
--
-- * 'gsamlprsSAMLMetadataDocument'
--
-- * 'gsamlprsStatus'
data GetSAMLProviderResponse = GetSAMLProviderResponse'
    { _gsamlprsCreateDate           :: !(Maybe ISO8601)
    , _gsamlprsValidUntil           :: !(Maybe ISO8601)
    , _gsamlprsSAMLMetadataDocument :: !(Maybe Text)
    , _gsamlprsStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetSAMLProviderResponse' smart constructor.
getSAMLProviderResponse :: Int -> GetSAMLProviderResponse
getSAMLProviderResponse pStatus =
    GetSAMLProviderResponse'
    { _gsamlprsCreateDate = Nothing
    , _gsamlprsValidUntil = Nothing
    , _gsamlprsSAMLMetadataDocument = Nothing
    , _gsamlprsStatus = pStatus
    }

-- | The date and time when the SAML provider was created.
gsamlprsCreateDate :: Lens' GetSAMLProviderResponse (Maybe UTCTime)
gsamlprsCreateDate = lens _gsamlprsCreateDate (\ s a -> s{_gsamlprsCreateDate = a}) . mapping _Time;

-- | The expiration date and time for the SAML provider.
gsamlprsValidUntil :: Lens' GetSAMLProviderResponse (Maybe UTCTime)
gsamlprsValidUntil = lens _gsamlprsValidUntil (\ s a -> s{_gsamlprsValidUntil = a}) . mapping _Time;

-- | The XML metadata document that includes information about an identity
-- provider.
gsamlprsSAMLMetadataDocument :: Lens' GetSAMLProviderResponse (Maybe Text)
gsamlprsSAMLMetadataDocument = lens _gsamlprsSAMLMetadataDocument (\ s a -> s{_gsamlprsSAMLMetadataDocument = a});

-- | FIXME: Undocumented member.
gsamlprsStatus :: Lens' GetSAMLProviderResponse Int
gsamlprsStatus = lens _gsamlprsStatus (\ s a -> s{_gsamlprsStatus = a});

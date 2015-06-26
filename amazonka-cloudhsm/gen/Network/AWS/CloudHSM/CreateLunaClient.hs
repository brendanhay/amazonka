{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudHSM.CreateLunaClient
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates an HSM client.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_CreateLunaClient.html>
module Network.AWS.CloudHSM.CreateLunaClient
    (
    -- * Request
      CreateLunaClient
    -- ** Request constructor
    , createLunaClient
    -- ** Request lenses
    , clcLabel
    , clcCertificate

    -- * Response
    , CreateLunaClientResponse
    -- ** Response constructor
    , createLunaClientResponse
    -- ** Response lenses
    , clcrClientARN
    , clcrStatusCode
    ) where

import Network.AWS.CloudHSM.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the inputs for the CreateLunaClient action.
--
-- /See:/ 'createLunaClient' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clcLabel'
--
-- * 'clcCertificate'
data CreateLunaClient = CreateLunaClient'{_clcLabel :: Maybe Text, _clcCertificate :: Text} deriving (Eq, Read, Show)

-- | 'CreateLunaClient' smart constructor.
createLunaClient :: Text -> CreateLunaClient
createLunaClient pCertificate = CreateLunaClient'{_clcLabel = Nothing, _clcCertificate = pCertificate};

-- | The label for the client.
clcLabel :: Lens' CreateLunaClient (Maybe Text)
clcLabel = lens _clcLabel (\ s a -> s{_clcLabel = a});

-- | The contents of a Base64-Encoded X.509 v3 certificate to be installed on
-- the HSMs used by this client.
clcCertificate :: Lens' CreateLunaClient Text
clcCertificate = lens _clcCertificate (\ s a -> s{_clcCertificate = a});

instance AWSRequest CreateLunaClient where
        type Sv CreateLunaClient = CloudHSM
        type Rs CreateLunaClient = CreateLunaClientResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateLunaClientResponse' <$>
                   (x .?> "ClientArn") <*> (pure (fromEnum s)))

instance ToHeaders CreateLunaClient where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.CreateLunaClient" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateLunaClient where
        toJSON CreateLunaClient'{..}
          = object
              ["Label" .= _clcLabel,
               "Certificate" .= _clcCertificate]

instance ToPath CreateLunaClient where
        toPath = const "/"

instance ToQuery CreateLunaClient where
        toQuery = const mempty

-- | Contains the output of the CreateLunaClient action.
--
-- /See:/ 'createLunaClientResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clcrClientARN'
--
-- * 'clcrStatusCode'
data CreateLunaClientResponse = CreateLunaClientResponse'{_clcrClientARN :: Maybe Text, _clcrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'CreateLunaClientResponse' smart constructor.
createLunaClientResponse :: Int -> CreateLunaClientResponse
createLunaClientResponse pStatusCode = CreateLunaClientResponse'{_clcrClientARN = Nothing, _clcrStatusCode = pStatusCode};

-- | The ARN of the client.
clcrClientARN :: Lens' CreateLunaClientResponse (Maybe Text)
clcrClientARN = lens _clcrClientARN (\ s a -> s{_clcrClientARN = a});

-- | FIXME: Undocumented member.
clcrStatusCode :: Lens' CreateLunaClientResponse Int
clcrStatusCode = lens _clcrStatusCode (\ s a -> s{_clcrStatusCode = a});

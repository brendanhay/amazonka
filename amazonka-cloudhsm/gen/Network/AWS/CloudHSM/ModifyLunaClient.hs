{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudHSM.ModifyLunaClient
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

-- | Modifies the certificate used by the client.
--
-- This action can potentially start a workflow to install the new
-- certificate on the client\'s HSMs.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_ModifyLunaClient.html>
module Network.AWS.CloudHSM.ModifyLunaClient
    (
    -- * Request
      ModifyLunaClient
    -- ** Request constructor
    , modifyLunaClient
    -- ** Request lenses
    , mlcClientARN
    , mlcCertificate

    -- * Response
    , ModifyLunaClientResponse
    -- ** Response constructor
    , modifyLunaClientResponse
    -- ** Response lenses
    , mlcrClientARN
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudHSM.Types

-- | /See:/ 'modifyLunaClient' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mlcClientARN'
--
-- * 'mlcCertificate'
data ModifyLunaClient = ModifyLunaClient'{_mlcClientARN :: Text, _mlcCertificate :: Text} deriving (Eq, Read, Show)

-- | 'ModifyLunaClient' smart constructor.
modifyLunaClient :: Text -> Text -> ModifyLunaClient
modifyLunaClient pClientARN pCertificate = ModifyLunaClient'{_mlcClientARN = pClientARN, _mlcCertificate = pCertificate};

-- | The ARN of the client.
mlcClientARN :: Lens' ModifyLunaClient Text
mlcClientARN = lens _mlcClientARN (\ s a -> s{_mlcClientARN = a});

-- | The new certificate for the client.
mlcCertificate :: Lens' ModifyLunaClient Text
mlcCertificate = lens _mlcCertificate (\ s a -> s{_mlcCertificate = a});

instance AWSRequest ModifyLunaClient where
        type Sv ModifyLunaClient = CloudHSM
        type Rs ModifyLunaClient = ModifyLunaClientResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ModifyLunaClientResponse' <$> x .?> "ClientArn")

instance ToHeaders ModifyLunaClient where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.ModifyLunaClient" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ModifyLunaClient where
        toJSON ModifyLunaClient'{..}
          = object
              ["ClientArn" .= _mlcClientARN,
               "Certificate" .= _mlcCertificate]

instance ToPath ModifyLunaClient where
        toPath = const "/"

instance ToQuery ModifyLunaClient where
        toQuery = const mempty

-- | /See:/ 'modifyLunaClientResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mlcrClientARN'
newtype ModifyLunaClientResponse = ModifyLunaClientResponse'{_mlcrClientARN :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ModifyLunaClientResponse' smart constructor.
modifyLunaClientResponse :: ModifyLunaClientResponse
modifyLunaClientResponse = ModifyLunaClientResponse'{_mlcrClientARN = Nothing};

-- | The ARN of the client.
mlcrClientARN :: Lens' ModifyLunaClientResponse (Maybe Text)
mlcrClientARN = lens _mlcrClientARN (\ s a -> s{_mlcrClientARN = a});

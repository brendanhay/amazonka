{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.CreateLunaClient
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates an HSM client.
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
    , clcrsClientARN
    , clcrsStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the CreateLunaClient action.
--
-- /See:/ 'createLunaClient' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clcLabel'
--
-- * 'clcCertificate'
data CreateLunaClient = CreateLunaClient'
    { _clcLabel       :: !(Maybe Text)
    , _clcCertificate :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateLunaClient' smart constructor.
createLunaClient :: Text -> CreateLunaClient
createLunaClient pCertificate_ =
    CreateLunaClient'
    { _clcLabel = Nothing
    , _clcCertificate = pCertificate_
    }

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
        request = postJSON "CreateLunaClient"
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
-- * 'clcrsClientARN'
--
-- * 'clcrsStatus'
data CreateLunaClientResponse = CreateLunaClientResponse'
    { _clcrsClientARN :: !(Maybe Text)
    , _clcrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateLunaClientResponse' smart constructor.
createLunaClientResponse :: Int -> CreateLunaClientResponse
createLunaClientResponse pStatus_ =
    CreateLunaClientResponse'
    { _clcrsClientARN = Nothing
    , _clcrsStatus = pStatus_
    }

-- | The ARN of the client.
clcrsClientARN :: Lens' CreateLunaClientResponse (Maybe Text)
clcrsClientARN = lens _clcrsClientARN (\ s a -> s{_clcrsClientARN = a});

-- | FIXME: Undocumented member.
clcrsStatus :: Lens' CreateLunaClientResponse Int
clcrsStatus = lens _clcrsStatus (\ s a -> s{_clcrsStatus = a});

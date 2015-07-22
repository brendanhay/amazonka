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
    , clcrqLabel
    , clcrqCertificate

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
-- * 'clcrqLabel'
--
-- * 'clcrqCertificate'
data CreateLunaClient = CreateLunaClient'
    { _clcrqLabel       :: !(Maybe Text)
    , _clcrqCertificate :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateLunaClient' smart constructor.
createLunaClient :: Text -> CreateLunaClient
createLunaClient pCertificate =
    CreateLunaClient'
    { _clcrqLabel = Nothing
    , _clcrqCertificate = pCertificate
    }

-- | The label for the client.
clcrqLabel :: Lens' CreateLunaClient (Maybe Text)
clcrqLabel = lens _clcrqLabel (\ s a -> s{_clcrqLabel = a});

-- | The contents of a Base64-Encoded X.509 v3 certificate to be installed on
-- the HSMs used by this client.
clcrqCertificate :: Lens' CreateLunaClient Text
clcrqCertificate = lens _clcrqCertificate (\ s a -> s{_clcrqCertificate = a});

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
              ["Label" .= _clcrqLabel,
               "Certificate" .= _clcrqCertificate]

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
createLunaClientResponse pStatus =
    CreateLunaClientResponse'
    { _clcrsClientARN = Nothing
    , _clcrsStatus = pStatus
    }

-- | The ARN of the client.
clcrsClientARN :: Lens' CreateLunaClientResponse (Maybe Text)
clcrsClientARN = lens _clcrsClientARN (\ s a -> s{_clcrsClientARN = a});

-- | FIXME: Undocumented member.
clcrsStatus :: Lens' CreateLunaClientResponse Int
clcrsStatus = lens _clcrsStatus (\ s a -> s{_clcrsStatus = a});

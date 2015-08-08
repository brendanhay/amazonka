{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.ModifyLunaClient
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Modifies the certificate used by the client.
--
-- This action can potentially start a workflow to install the new
-- certificate on the client\'s HSMs.
--
-- /See:/ <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_ModifyLunaClient.html AWS API Reference> for ModifyLunaClient.
module Network.AWS.CloudHSM.ModifyLunaClient
    (
    -- * Creating a Request
      ModifyLunaClient
    , modifyLunaClient
    -- * Request Lenses
    , mlcClientARN
    , mlcCertificate

    -- * Destructuring the Response
    , ModifyLunaClientResponse
    , modifyLunaClientResponse
    -- * Response Lenses
    , mlcrsClientARN
    , mlcrsStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'modifyLunaClient' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mlcClientARN'
--
-- * 'mlcCertificate'
data ModifyLunaClient = ModifyLunaClient'
    { _mlcClientARN   :: !Text
    , _mlcCertificate :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyLunaClient' smart constructor.
modifyLunaClient :: Text -> Text -> ModifyLunaClient
modifyLunaClient pClientARN_ pCertificate_ =
    ModifyLunaClient'
    { _mlcClientARN = pClientARN_
    , _mlcCertificate = pCertificate_
    }

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
                 ModifyLunaClientResponse' <$>
                   (x .?> "ClientArn") <*> (pure (fromEnum s)))

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
-- * 'mlcrsClientARN'
--
-- * 'mlcrsStatus'
data ModifyLunaClientResponse = ModifyLunaClientResponse'
    { _mlcrsClientARN :: !(Maybe Text)
    , _mlcrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyLunaClientResponse' smart constructor.
modifyLunaClientResponse :: Int -> ModifyLunaClientResponse
modifyLunaClientResponse pStatus_ =
    ModifyLunaClientResponse'
    { _mlcrsClientARN = Nothing
    , _mlcrsStatus = pStatus_
    }

-- | The ARN of the client.
mlcrsClientARN :: Lens' ModifyLunaClientResponse (Maybe Text)
mlcrsClientARN = lens _mlcrsClientARN (\ s a -> s{_mlcrsClientARN = a});

-- | Undocumented member.
mlcrsStatus :: Lens' ModifyLunaClientResponse Int
mlcrsStatus = lens _mlcrsStatus (\ s a -> s{_mlcrsStatus = a});

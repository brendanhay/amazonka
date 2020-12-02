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
-- Module      : Network.AWS.CloudHSM.ModifyLunaClient
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
--
-- Modifies the certificate used by the client.
--
-- This action can potentially start a workflow to install the new certificate on the client's HSMs.
--
module Network.AWS.CloudHSM.ModifyLunaClient
    (
    -- * Creating a Request
      modifyLunaClient
    , ModifyLunaClient
    -- * Request Lenses
    , mlcClientARN
    , mlcCertificate

    -- * Destructuring the Response
    , modifyLunaClientResponse
    , ModifyLunaClientResponse
    -- * Response Lenses
    , mlcrsClientARN
    , mlcrsResponseStatus
    ) where

import Network.AWS.CloudHSM.Types
import Network.AWS.CloudHSM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyLunaClient' smart constructor.
data ModifyLunaClient = ModifyLunaClient'
  { _mlcClientARN   :: !Text
  , _mlcCertificate :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyLunaClient' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mlcClientARN' - The ARN of the client.
--
-- * 'mlcCertificate' - The new certificate for the client.
modifyLunaClient
    :: Text -- ^ 'mlcClientARN'
    -> Text -- ^ 'mlcCertificate'
    -> ModifyLunaClient
modifyLunaClient pClientARN_ pCertificate_ =
  ModifyLunaClient'
    {_mlcClientARN = pClientARN_, _mlcCertificate = pCertificate_}


-- | The ARN of the client.
mlcClientARN :: Lens' ModifyLunaClient Text
mlcClientARN = lens _mlcClientARN (\ s a -> s{_mlcClientARN = a})

-- | The new certificate for the client.
mlcCertificate :: Lens' ModifyLunaClient Text
mlcCertificate = lens _mlcCertificate (\ s a -> s{_mlcCertificate = a})

instance AWSRequest ModifyLunaClient where
        type Rs ModifyLunaClient = ModifyLunaClientResponse
        request = postJSON cloudHSM
        response
          = receiveJSON
              (\ s h x ->
                 ModifyLunaClientResponse' <$>
                   (x .?> "ClientArn") <*> (pure (fromEnum s)))

instance Hashable ModifyLunaClient where

instance NFData ModifyLunaClient where

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
              (catMaybes
                 [Just ("ClientArn" .= _mlcClientARN),
                  Just ("Certificate" .= _mlcCertificate)])

instance ToPath ModifyLunaClient where
        toPath = const "/"

instance ToQuery ModifyLunaClient where
        toQuery = const mempty

-- | /See:/ 'modifyLunaClientResponse' smart constructor.
data ModifyLunaClientResponse = ModifyLunaClientResponse'
  { _mlcrsClientARN      :: !(Maybe Text)
  , _mlcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyLunaClientResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mlcrsClientARN' - The ARN of the client.
--
-- * 'mlcrsResponseStatus' - -- | The response status code.
modifyLunaClientResponse
    :: Int -- ^ 'mlcrsResponseStatus'
    -> ModifyLunaClientResponse
modifyLunaClientResponse pResponseStatus_ =
  ModifyLunaClientResponse'
    {_mlcrsClientARN = Nothing, _mlcrsResponseStatus = pResponseStatus_}


-- | The ARN of the client.
mlcrsClientARN :: Lens' ModifyLunaClientResponse (Maybe Text)
mlcrsClientARN = lens _mlcrsClientARN (\ s a -> s{_mlcrsClientARN = a})

-- | -- | The response status code.
mlcrsResponseStatus :: Lens' ModifyLunaClientResponse Int
mlcrsResponseStatus = lens _mlcrsResponseStatus (\ s a -> s{_mlcrsResponseStatus = a})

instance NFData ModifyLunaClientResponse where

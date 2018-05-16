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
-- Module      : Network.AWS.CloudHSM.CreateLunaClient
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
-- Creates an HSM client.
--
module Network.AWS.CloudHSM.CreateLunaClient
    (
    -- * Creating a Request
      createLunaClient
    , CreateLunaClient
    -- * Request Lenses
    , clcLabel
    , clcCertificate

    -- * Destructuring the Response
    , createLunaClientResponse
    , CreateLunaClientResponse
    -- * Response Lenses
    , clcrsClientARN
    , clcrsResponseStatus
    ) where

import Network.AWS.CloudHSM.Types
import Network.AWS.CloudHSM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the inputs for the 'CreateLunaClient' action.
--
--
--
-- /See:/ 'createLunaClient' smart constructor.
data CreateLunaClient = CreateLunaClient'
  { _clcLabel       :: !(Maybe Text)
  , _clcCertificate :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLunaClient' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clcLabel' - The label for the client.
--
-- * 'clcCertificate' - The contents of a Base64-Encoded X.509 v3 certificate to be installed on the HSMs used by this client.
createLunaClient
    :: Text -- ^ 'clcCertificate'
    -> CreateLunaClient
createLunaClient pCertificate_ =
  CreateLunaClient' {_clcLabel = Nothing, _clcCertificate = pCertificate_}


-- | The label for the client.
clcLabel :: Lens' CreateLunaClient (Maybe Text)
clcLabel = lens _clcLabel (\ s a -> s{_clcLabel = a})

-- | The contents of a Base64-Encoded X.509 v3 certificate to be installed on the HSMs used by this client.
clcCertificate :: Lens' CreateLunaClient Text
clcCertificate = lens _clcCertificate (\ s a -> s{_clcCertificate = a})

instance AWSRequest CreateLunaClient where
        type Rs CreateLunaClient = CreateLunaClientResponse
        request = postJSON cloudHSM
        response
          = receiveJSON
              (\ s h x ->
                 CreateLunaClientResponse' <$>
                   (x .?> "ClientArn") <*> (pure (fromEnum s)))

instance Hashable CreateLunaClient where

instance NFData CreateLunaClient where

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
              (catMaybes
                 [("Label" .=) <$> _clcLabel,
                  Just ("Certificate" .= _clcCertificate)])

instance ToPath CreateLunaClient where
        toPath = const "/"

instance ToQuery CreateLunaClient where
        toQuery = const mempty

-- | Contains the output of the 'CreateLunaClient' action.
--
--
--
-- /See:/ 'createLunaClientResponse' smart constructor.
data CreateLunaClientResponse = CreateLunaClientResponse'
  { _clcrsClientARN      :: !(Maybe Text)
  , _clcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLunaClientResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clcrsClientARN' - The ARN of the client.
--
-- * 'clcrsResponseStatus' - -- | The response status code.
createLunaClientResponse
    :: Int -- ^ 'clcrsResponseStatus'
    -> CreateLunaClientResponse
createLunaClientResponse pResponseStatus_ =
  CreateLunaClientResponse'
    {_clcrsClientARN = Nothing, _clcrsResponseStatus = pResponseStatus_}


-- | The ARN of the client.
clcrsClientARN :: Lens' CreateLunaClientResponse (Maybe Text)
clcrsClientARN = lens _clcrsClientARN (\ s a -> s{_clcrsClientARN = a})

-- | -- | The response status code.
clcrsResponseStatus :: Lens' CreateLunaClientResponse Int
clcrsResponseStatus = lens _clcrsResponseStatus (\ s a -> s{_clcrsResponseStatus = a})

instance NFData CreateLunaClientResponse where

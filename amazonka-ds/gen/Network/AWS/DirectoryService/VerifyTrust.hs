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
-- Module      : Network.AWS.DirectoryService.VerifyTrust
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Directory Service for Microsoft Active Directory allows you to configure and verify trust relationships.
--
--
-- This action verifies a trust relationship between your Microsoft AD in the AWS cloud and an external domain.
--
module Network.AWS.DirectoryService.VerifyTrust
    (
    -- * Creating a Request
      verifyTrust
    , VerifyTrust
    -- * Request Lenses
    , vtTrustId

    -- * Destructuring the Response
    , verifyTrustResponse
    , VerifyTrustResponse
    -- * Response Lenses
    , vtrsTrustId
    , vtrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Initiates the verification of an existing trust relationship between a Microsoft AD in the AWS cloud and an external domain.
--
--
--
-- /See:/ 'verifyTrust' smart constructor.
newtype VerifyTrust = VerifyTrust'
  { _vtTrustId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VerifyTrust' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vtTrustId' - The unique Trust ID of the trust relationship to verify.
verifyTrust
    :: Text -- ^ 'vtTrustId'
    -> VerifyTrust
verifyTrust pTrustId_ = VerifyTrust' {_vtTrustId = pTrustId_}


-- | The unique Trust ID of the trust relationship to verify.
vtTrustId :: Lens' VerifyTrust Text
vtTrustId = lens _vtTrustId (\ s a -> s{_vtTrustId = a})

instance AWSRequest VerifyTrust where
        type Rs VerifyTrust = VerifyTrustResponse
        request = postJSON directoryService
        response
          = receiveJSON
              (\ s h x ->
                 VerifyTrustResponse' <$>
                   (x .?> "TrustId") <*> (pure (fromEnum s)))

instance Hashable VerifyTrust where

instance NFData VerifyTrust where

instance ToHeaders VerifyTrust where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.VerifyTrust" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON VerifyTrust where
        toJSON VerifyTrust'{..}
          = object (catMaybes [Just ("TrustId" .= _vtTrustId)])

instance ToPath VerifyTrust where
        toPath = const "/"

instance ToQuery VerifyTrust where
        toQuery = const mempty

-- | Result of a VerifyTrust request.
--
--
--
-- /See:/ 'verifyTrustResponse' smart constructor.
data VerifyTrustResponse = VerifyTrustResponse'
  { _vtrsTrustId        :: !(Maybe Text)
  , _vtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VerifyTrustResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vtrsTrustId' - The unique Trust ID of the trust relationship that was verified.
--
-- * 'vtrsResponseStatus' - -- | The response status code.
verifyTrustResponse
    :: Int -- ^ 'vtrsResponseStatus'
    -> VerifyTrustResponse
verifyTrustResponse pResponseStatus_ =
  VerifyTrustResponse'
    {_vtrsTrustId = Nothing, _vtrsResponseStatus = pResponseStatus_}


-- | The unique Trust ID of the trust relationship that was verified.
vtrsTrustId :: Lens' VerifyTrustResponse (Maybe Text)
vtrsTrustId = lens _vtrsTrustId (\ s a -> s{_vtrsTrustId = a})

-- | -- | The response status code.
vtrsResponseStatus :: Lens' VerifyTrustResponse Int
vtrsResponseStatus = lens _vtrsResponseStatus (\ s a -> s{_vtrsResponseStatus = a})

instance NFData VerifyTrustResponse where

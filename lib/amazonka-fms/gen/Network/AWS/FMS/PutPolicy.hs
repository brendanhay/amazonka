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
-- Module      : Network.AWS.FMS.PutPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Firewall Manager policy.
--
--
module Network.AWS.FMS.PutPolicy
    (
    -- * Creating a Request
      putPolicy
    , PutPolicy
    -- * Request Lenses
    , ppPolicy

    -- * Destructuring the Response
    , putPolicyResponse
    , PutPolicyResponse
    -- * Response Lenses
    , pprsPolicyARN
    , pprsPolicy
    , pprsResponseStatus
    ) where

import Network.AWS.FMS.Types
import Network.AWS.FMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putPolicy' smart constructor.
newtype PutPolicy = PutPolicy'
  { _ppPolicy :: Policy
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppPolicy' - The details of the AWS Firewall Manager policy to be created.
putPolicy
    :: Policy -- ^ 'ppPolicy'
    -> PutPolicy
putPolicy pPolicy_ = PutPolicy' {_ppPolicy = pPolicy_}


-- | The details of the AWS Firewall Manager policy to be created.
ppPolicy :: Lens' PutPolicy Policy
ppPolicy = lens _ppPolicy (\ s a -> s{_ppPolicy = a})

instance AWSRequest PutPolicy where
        type Rs PutPolicy = PutPolicyResponse
        request = postJSON fms
        response
          = receiveJSON
              (\ s h x ->
                 PutPolicyResponse' <$>
                   (x .?> "PolicyArn") <*> (x .?> "Policy") <*>
                     (pure (fromEnum s)))

instance Hashable PutPolicy where

instance NFData PutPolicy where

instance ToHeaders PutPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSFMS_20180101.PutPolicy" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutPolicy where
        toJSON PutPolicy'{..}
          = object (catMaybes [Just ("Policy" .= _ppPolicy)])

instance ToPath PutPolicy where
        toPath = const "/"

instance ToQuery PutPolicy where
        toQuery = const mempty

-- | /See:/ 'putPolicyResponse' smart constructor.
data PutPolicyResponse = PutPolicyResponse'
  { _pprsPolicyARN      :: !(Maybe Text)
  , _pprsPolicy         :: !(Maybe Policy)
  , _pprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pprsPolicyARN' - The Amazon Resource Name (ARN) of the policy that was created.
--
-- * 'pprsPolicy' - The details of the AWS Firewall Manager policy that was created.
--
-- * 'pprsResponseStatus' - -- | The response status code.
putPolicyResponse
    :: Int -- ^ 'pprsResponseStatus'
    -> PutPolicyResponse
putPolicyResponse pResponseStatus_ =
  PutPolicyResponse'
    { _pprsPolicyARN = Nothing
    , _pprsPolicy = Nothing
    , _pprsResponseStatus = pResponseStatus_
    }


-- | The Amazon Resource Name (ARN) of the policy that was created.
pprsPolicyARN :: Lens' PutPolicyResponse (Maybe Text)
pprsPolicyARN = lens _pprsPolicyARN (\ s a -> s{_pprsPolicyARN = a})

-- | The details of the AWS Firewall Manager policy that was created.
pprsPolicy :: Lens' PutPolicyResponse (Maybe Policy)
pprsPolicy = lens _pprsPolicy (\ s a -> s{_pprsPolicy = a})

-- | -- | The response status code.
pprsResponseStatus :: Lens' PutPolicyResponse Int
pprsResponseStatus = lens _pprsResponseStatus (\ s a -> s{_pprsResponseStatus = a})

instance NFData PutPolicyResponse where

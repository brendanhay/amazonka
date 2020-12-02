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
-- Module      : Network.AWS.WAF.PutPermissionPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a IAM policy to the specified resource. The only supported use for this action is to share a RuleGroup across accounts.
--
--
-- The @PutPermissionPolicy@ is subject to the following restrictions:
--
--     * You can attach only one policy with each @PutPermissionPolicy@ request.
--
--     * The policy must include an @Effect@ , @Action@ and @Principal@ .
--
--     * @Effect@ must specify @Allow@ .
--
--     * The @Action@ in the policy must be @waf:UpdateWebACL@ and @waf-regional:UpdateWebACL@ . Any extra or wildcard actions in the policy will be rejected.
--
--     * The policy cannot include a @Resource@ parameter.
--
--     * The ARN in the request must be a valid WAF RuleGroup ARN and the RuleGroup must exist in the same region.
--
--     * The user making the request must be the owner of the RuleGroup.
--
--     * Your policy must be composed using IAM Policy version 2012-10-17.
--
--
--
-- For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html IAM Policies> .
--
-- An example of a valid policy parameter is shown in the Examples section below.
--
module Network.AWS.WAF.PutPermissionPolicy
    (
    -- * Creating a Request
      putPermissionPolicy
    , PutPermissionPolicy
    -- * Request Lenses
    , pppResourceARN
    , pppPolicy

    -- * Destructuring the Response
    , putPermissionPolicyResponse
    , PutPermissionPolicyResponse
    -- * Response Lenses
    , ppprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'putPermissionPolicy' smart constructor.
data PutPermissionPolicy = PutPermissionPolicy'
  { _pppResourceARN :: !Text
  , _pppPolicy      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutPermissionPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pppResourceARN' - The Amazon Resource Name (ARN) of the RuleGroup to which you want to attach the policy.
--
-- * 'pppPolicy' - The policy to attach to the specified RuleGroup.
putPermissionPolicy
    :: Text -- ^ 'pppResourceARN'
    -> Text -- ^ 'pppPolicy'
    -> PutPermissionPolicy
putPermissionPolicy pResourceARN_ pPolicy_ =
  PutPermissionPolicy' {_pppResourceARN = pResourceARN_, _pppPolicy = pPolicy_}


-- | The Amazon Resource Name (ARN) of the RuleGroup to which you want to attach the policy.
pppResourceARN :: Lens' PutPermissionPolicy Text
pppResourceARN = lens _pppResourceARN (\ s a -> s{_pppResourceARN = a})

-- | The policy to attach to the specified RuleGroup.
pppPolicy :: Lens' PutPermissionPolicy Text
pppPolicy = lens _pppPolicy (\ s a -> s{_pppPolicy = a})

instance AWSRequest PutPermissionPolicy where
        type Rs PutPermissionPolicy =
             PutPermissionPolicyResponse
        request = postJSON waf
        response
          = receiveEmpty
              (\ s h x ->
                 PutPermissionPolicyResponse' <$> (pure (fromEnum s)))

instance Hashable PutPermissionPolicy where

instance NFData PutPermissionPolicy where

instance ToHeaders PutPermissionPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.PutPermissionPolicy" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutPermissionPolicy where
        toJSON PutPermissionPolicy'{..}
          = object
              (catMaybes
                 [Just ("ResourceArn" .= _pppResourceARN),
                  Just ("Policy" .= _pppPolicy)])

instance ToPath PutPermissionPolicy where
        toPath = const "/"

instance ToQuery PutPermissionPolicy where
        toQuery = const mempty

-- | /See:/ 'putPermissionPolicyResponse' smart constructor.
newtype PutPermissionPolicyResponse = PutPermissionPolicyResponse'
  { _ppprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutPermissionPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppprsResponseStatus' - -- | The response status code.
putPermissionPolicyResponse
    :: Int -- ^ 'ppprsResponseStatus'
    -> PutPermissionPolicyResponse
putPermissionPolicyResponse pResponseStatus_ =
  PutPermissionPolicyResponse' {_ppprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ppprsResponseStatus :: Lens' PutPermissionPolicyResponse Int
ppprsResponseStatus = lens _ppprsResponseStatus (\ s a -> s{_ppprsResponseStatus = a})

instance NFData PutPermissionPolicyResponse where

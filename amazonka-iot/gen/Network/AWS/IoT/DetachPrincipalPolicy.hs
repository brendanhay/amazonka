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
-- Module      : Network.AWS.IoT.DetachPrincipalPolicy
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified policy from the specified certificate.
--
--
module Network.AWS.IoT.DetachPrincipalPolicy
    (
    -- * Creating a Request
      detachPrincipalPolicy
    , DetachPrincipalPolicy
    -- * Request Lenses
    , dppPolicyName
    , dppPrincipal

    -- * Destructuring the Response
    , detachPrincipalPolicyResponse
    , DetachPrincipalPolicyResponse
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the DetachPrincipalPolicy operation.
--
--
--
-- /See:/ 'detachPrincipalPolicy' smart constructor.
data DetachPrincipalPolicy = DetachPrincipalPolicy'
  { _dppPolicyName :: !Text
  , _dppPrincipal  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachPrincipalPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dppPolicyName' - The name of the policy to detach.
--
-- * 'dppPrincipal' - The principal. If the principal is a certificate, specify the certificate ARN. If the principal is an Amazon Cognito identity, specify the identity ID.
detachPrincipalPolicy
    :: Text -- ^ 'dppPolicyName'
    -> Text -- ^ 'dppPrincipal'
    -> DetachPrincipalPolicy
detachPrincipalPolicy pPolicyName_ pPrincipal_ =
  DetachPrincipalPolicy'
  {_dppPolicyName = pPolicyName_, _dppPrincipal = pPrincipal_}


-- | The name of the policy to detach.
dppPolicyName :: Lens' DetachPrincipalPolicy Text
dppPolicyName = lens _dppPolicyName (\ s a -> s{_dppPolicyName = a});

-- | The principal. If the principal is a certificate, specify the certificate ARN. If the principal is an Amazon Cognito identity, specify the identity ID.
dppPrincipal :: Lens' DetachPrincipalPolicy Text
dppPrincipal = lens _dppPrincipal (\ s a -> s{_dppPrincipal = a});

instance AWSRequest DetachPrincipalPolicy where
        type Rs DetachPrincipalPolicy =
             DetachPrincipalPolicyResponse
        request = delete ioT
        response = receiveNull DetachPrincipalPolicyResponse'

instance Hashable DetachPrincipalPolicy where

instance NFData DetachPrincipalPolicy where

instance ToHeaders DetachPrincipalPolicy where
        toHeaders DetachPrincipalPolicy'{..}
          = mconcat ["x-amzn-iot-principal" =# _dppPrincipal]

instance ToPath DetachPrincipalPolicy where
        toPath DetachPrincipalPolicy'{..}
          = mconcat
              ["/principal-policies/", toBS _dppPolicyName]

instance ToQuery DetachPrincipalPolicy where
        toQuery = const mempty

-- | /See:/ 'detachPrincipalPolicyResponse' smart constructor.
data DetachPrincipalPolicyResponse =
  DetachPrincipalPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachPrincipalPolicyResponse' with the minimum fields required to make a request.
--
detachPrincipalPolicyResponse
    :: DetachPrincipalPolicyResponse
detachPrincipalPolicyResponse = DetachPrincipalPolicyResponse'


instance NFData DetachPrincipalPolicyResponse where

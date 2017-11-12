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
-- Module      : Network.AWS.IoT.AttachPrincipalPolicy
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified policy to the specified principal (certificate or other credential).
--
--
module Network.AWS.IoT.AttachPrincipalPolicy
    (
    -- * Creating a Request
      attachPrincipalPolicy
    , AttachPrincipalPolicy
    -- * Request Lenses
    , appPolicyName
    , appPrincipal

    -- * Destructuring the Response
    , attachPrincipalPolicyResponse
    , AttachPrincipalPolicyResponse
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the AttachPrincipalPolicy operation.
--
--
--
-- /See:/ 'attachPrincipalPolicy' smart constructor.
data AttachPrincipalPolicy = AttachPrincipalPolicy'
  { _appPolicyName :: !Text
  , _appPrincipal  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachPrincipalPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'appPolicyName' - The policy name.
--
-- * 'appPrincipal' - The principal, which can be a certificate ARN (as returned from the CreateCertificate operation) or an Amazon Cognito ID.
attachPrincipalPolicy
    :: Text -- ^ 'appPolicyName'
    -> Text -- ^ 'appPrincipal'
    -> AttachPrincipalPolicy
attachPrincipalPolicy pPolicyName_ pPrincipal_ =
  AttachPrincipalPolicy'
  {_appPolicyName = pPolicyName_, _appPrincipal = pPrincipal_}


-- | The policy name.
appPolicyName :: Lens' AttachPrincipalPolicy Text
appPolicyName = lens _appPolicyName (\ s a -> s{_appPolicyName = a});

-- | The principal, which can be a certificate ARN (as returned from the CreateCertificate operation) or an Amazon Cognito ID.
appPrincipal :: Lens' AttachPrincipalPolicy Text
appPrincipal = lens _appPrincipal (\ s a -> s{_appPrincipal = a});

instance AWSRequest AttachPrincipalPolicy where
        type Rs AttachPrincipalPolicy =
             AttachPrincipalPolicyResponse
        request = putJSON ioT
        response = receiveNull AttachPrincipalPolicyResponse'

instance Hashable AttachPrincipalPolicy where

instance NFData AttachPrincipalPolicy where

instance ToHeaders AttachPrincipalPolicy where
        toHeaders AttachPrincipalPolicy'{..}
          = mconcat ["x-amzn-iot-principal" =# _appPrincipal]

instance ToJSON AttachPrincipalPolicy where
        toJSON = const (Object mempty)

instance ToPath AttachPrincipalPolicy where
        toPath AttachPrincipalPolicy'{..}
          = mconcat
              ["/principal-policies/", toBS _appPolicyName]

instance ToQuery AttachPrincipalPolicy where
        toQuery = const mempty

-- | /See:/ 'attachPrincipalPolicyResponse' smart constructor.
data AttachPrincipalPolicyResponse =
  AttachPrincipalPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachPrincipalPolicyResponse' with the minimum fields required to make a request.
--
attachPrincipalPolicyResponse
    :: AttachPrincipalPolicyResponse
attachPrincipalPolicyResponse = AttachPrincipalPolicyResponse'


instance NFData AttachPrincipalPolicyResponse where

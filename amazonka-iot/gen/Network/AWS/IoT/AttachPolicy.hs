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
-- Module      : Network.AWS.IoT.AttachPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a policy to the specified target.
--
--
module Network.AWS.IoT.AttachPolicy
    (
    -- * Creating a Request
      attachPolicy
    , AttachPolicy
    -- * Request Lenses
    , apPolicyName
    , apTarget

    -- * Destructuring the Response
    , attachPolicyResponse
    , AttachPolicyResponse
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'attachPolicy' smart constructor.
data AttachPolicy = AttachPolicy'
  { _apPolicyName :: !Text
  , _apTarget     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apPolicyName' - The name of the policy to attach.
--
-- * 'apTarget' - The identity to which the policy is attached.
attachPolicy
    :: Text -- ^ 'apPolicyName'
    -> Text -- ^ 'apTarget'
    -> AttachPolicy
attachPolicy pPolicyName_ pTarget_ =
  AttachPolicy' {_apPolicyName = pPolicyName_, _apTarget = pTarget_}


-- | The name of the policy to attach.
apPolicyName :: Lens' AttachPolicy Text
apPolicyName = lens _apPolicyName (\ s a -> s{_apPolicyName = a})

-- | The identity to which the policy is attached.
apTarget :: Lens' AttachPolicy Text
apTarget = lens _apTarget (\ s a -> s{_apTarget = a})

instance AWSRequest AttachPolicy where
        type Rs AttachPolicy = AttachPolicyResponse
        request = putJSON ioT
        response = receiveNull AttachPolicyResponse'

instance Hashable AttachPolicy where

instance NFData AttachPolicy where

instance ToHeaders AttachPolicy where
        toHeaders = const mempty

instance ToJSON AttachPolicy where
        toJSON AttachPolicy'{..}
          = object (catMaybes [Just ("target" .= _apTarget)])

instance ToPath AttachPolicy where
        toPath AttachPolicy'{..}
          = mconcat ["/target-policies/", toBS _apPolicyName]

instance ToQuery AttachPolicy where
        toQuery = const mempty

-- | /See:/ 'attachPolicyResponse' smart constructor.
data AttachPolicyResponse =
  AttachPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachPolicyResponse' with the minimum fields required to make a request.
--
attachPolicyResponse
    :: AttachPolicyResponse
attachPolicyResponse = AttachPolicyResponse'


instance NFData AttachPolicyResponse where

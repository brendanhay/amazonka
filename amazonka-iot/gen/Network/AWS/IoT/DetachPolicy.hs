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
-- Module      : Network.AWS.IoT.DetachPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a policy from the specified target.
--
--
module Network.AWS.IoT.DetachPolicy
    (
    -- * Creating a Request
      detachPolicy
    , DetachPolicy
    -- * Request Lenses
    , dPolicyName
    , dTarget

    -- * Destructuring the Response
    , detachPolicyResponse
    , DetachPolicyResponse
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detachPolicy' smart constructor.
data DetachPolicy = DetachPolicy'
  { _dPolicyName :: !Text
  , _dTarget     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dPolicyName' - The policy to detach.
--
-- * 'dTarget' - The target from which the policy will be detached.
detachPolicy
    :: Text -- ^ 'dPolicyName'
    -> Text -- ^ 'dTarget'
    -> DetachPolicy
detachPolicy pPolicyName_ pTarget_ =
  DetachPolicy' {_dPolicyName = pPolicyName_, _dTarget = pTarget_}


-- | The policy to detach.
dPolicyName :: Lens' DetachPolicy Text
dPolicyName = lens _dPolicyName (\ s a -> s{_dPolicyName = a})

-- | The target from which the policy will be detached.
dTarget :: Lens' DetachPolicy Text
dTarget = lens _dTarget (\ s a -> s{_dTarget = a})

instance AWSRequest DetachPolicy where
        type Rs DetachPolicy = DetachPolicyResponse
        request = postJSON ioT
        response = receiveNull DetachPolicyResponse'

instance Hashable DetachPolicy where

instance NFData DetachPolicy where

instance ToHeaders DetachPolicy where
        toHeaders = const mempty

instance ToJSON DetachPolicy where
        toJSON DetachPolicy'{..}
          = object (catMaybes [Just ("target" .= _dTarget)])

instance ToPath DetachPolicy where
        toPath DetachPolicy'{..}
          = mconcat ["/target-policies/", toBS _dPolicyName]

instance ToQuery DetachPolicy where
        toQuery = const mempty

-- | /See:/ 'detachPolicyResponse' smart constructor.
data DetachPolicyResponse =
  DetachPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachPolicyResponse' with the minimum fields required to make a request.
--
detachPolicyResponse
    :: DetachPolicyResponse
detachPolicyResponse = DetachPolicyResponse'


instance NFData DetachPolicyResponse where

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
-- Module      : Network.AWS.CloudDirectory.DetachPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a policy from an object.
--
--
module Network.AWS.CloudDirectory.DetachPolicy
    (
    -- * Creating a Request
      detachPolicy
    , DetachPolicy
    -- * Request Lenses
    , dpDirectoryARN
    , dpPolicyReference
    , dpObjectReference

    -- * Destructuring the Response
    , detachPolicyResponse
    , DetachPolicyResponse
    -- * Response Lenses
    , dprsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detachPolicy' smart constructor.
data DetachPolicy = DetachPolicy'
  { _dpDirectoryARN    :: !Text
  , _dpPolicyReference :: !ObjectReference
  , _dpObjectReference :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpDirectoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where both objects reside. For more information, see 'arns' .
--
-- * 'dpPolicyReference' - Reference that identifies the policy object.
--
-- * 'dpObjectReference' - Reference that identifies the object whose policy object will be detached.
detachPolicy
    :: Text -- ^ 'dpDirectoryARN'
    -> ObjectReference -- ^ 'dpPolicyReference'
    -> ObjectReference -- ^ 'dpObjectReference'
    -> DetachPolicy
detachPolicy pDirectoryARN_ pPolicyReference_ pObjectReference_ =
  DetachPolicy'
    { _dpDirectoryARN = pDirectoryARN_
    , _dpPolicyReference = pPolicyReference_
    , _dpObjectReference = pObjectReference_
    }


-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where both objects reside. For more information, see 'arns' .
dpDirectoryARN :: Lens' DetachPolicy Text
dpDirectoryARN = lens _dpDirectoryARN (\ s a -> s{_dpDirectoryARN = a})

-- | Reference that identifies the policy object.
dpPolicyReference :: Lens' DetachPolicy ObjectReference
dpPolicyReference = lens _dpPolicyReference (\ s a -> s{_dpPolicyReference = a})

-- | Reference that identifies the object whose policy object will be detached.
dpObjectReference :: Lens' DetachPolicy ObjectReference
dpObjectReference = lens _dpObjectReference (\ s a -> s{_dpObjectReference = a})

instance AWSRequest DetachPolicy where
        type Rs DetachPolicy = DetachPolicyResponse
        request = putJSON cloudDirectory
        response
          = receiveEmpty
              (\ s h x ->
                 DetachPolicyResponse' <$> (pure (fromEnum s)))

instance Hashable DetachPolicy where

instance NFData DetachPolicy where

instance ToHeaders DetachPolicy where
        toHeaders DetachPolicy'{..}
          = mconcat ["x-amz-data-partition" =# _dpDirectoryARN]

instance ToJSON DetachPolicy where
        toJSON DetachPolicy'{..}
          = object
              (catMaybes
                 [Just ("PolicyReference" .= _dpPolicyReference),
                  Just ("ObjectReference" .= _dpObjectReference)])

instance ToPath DetachPolicy where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/policy/detach"

instance ToQuery DetachPolicy where
        toQuery = const mempty

-- | /See:/ 'detachPolicyResponse' smart constructor.
newtype DetachPolicyResponse = DetachPolicyResponse'
  { _dprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dprsResponseStatus' - -- | The response status code.
detachPolicyResponse
    :: Int -- ^ 'dprsResponseStatus'
    -> DetachPolicyResponse
detachPolicyResponse pResponseStatus_ =
  DetachPolicyResponse' {_dprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dprsResponseStatus :: Lens' DetachPolicyResponse Int
dprsResponseStatus = lens _dprsResponseStatus (\ s a -> s{_dprsResponseStatus = a})

instance NFData DetachPolicyResponse where

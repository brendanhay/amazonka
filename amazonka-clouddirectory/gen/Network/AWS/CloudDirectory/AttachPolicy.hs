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
-- Module      : Network.AWS.CloudDirectory.AttachPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a policy object to a regular object. An object can have a limited number of attached policies.
--
--
module Network.AWS.CloudDirectory.AttachPolicy
    (
    -- * Creating a Request
      attachPolicy
    , AttachPolicy
    -- * Request Lenses
    , apDirectoryARN
    , apPolicyReference
    , apObjectReference

    -- * Destructuring the Response
    , attachPolicyResponse
    , AttachPolicyResponse
    -- * Response Lenses
    , aprsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'attachPolicy' smart constructor.
data AttachPolicy = AttachPolicy'
  { _apDirectoryARN    :: !Text
  , _apPolicyReference :: !ObjectReference
  , _apObjectReference :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apDirectoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where both objects reside. For more information, see 'arns' .
--
-- * 'apPolicyReference' - The reference that is associated with the policy object.
--
-- * 'apObjectReference' - The reference that identifies the object to which the policy will be attached.
attachPolicy
    :: Text -- ^ 'apDirectoryARN'
    -> ObjectReference -- ^ 'apPolicyReference'
    -> ObjectReference -- ^ 'apObjectReference'
    -> AttachPolicy
attachPolicy pDirectoryARN_ pPolicyReference_ pObjectReference_ =
  AttachPolicy'
    { _apDirectoryARN = pDirectoryARN_
    , _apPolicyReference = pPolicyReference_
    , _apObjectReference = pObjectReference_
    }


-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where both objects reside. For more information, see 'arns' .
apDirectoryARN :: Lens' AttachPolicy Text
apDirectoryARN = lens _apDirectoryARN (\ s a -> s{_apDirectoryARN = a})

-- | The reference that is associated with the policy object.
apPolicyReference :: Lens' AttachPolicy ObjectReference
apPolicyReference = lens _apPolicyReference (\ s a -> s{_apPolicyReference = a})

-- | The reference that identifies the object to which the policy will be attached.
apObjectReference :: Lens' AttachPolicy ObjectReference
apObjectReference = lens _apObjectReference (\ s a -> s{_apObjectReference = a})

instance AWSRequest AttachPolicy where
        type Rs AttachPolicy = AttachPolicyResponse
        request = putJSON cloudDirectory
        response
          = receiveEmpty
              (\ s h x ->
                 AttachPolicyResponse' <$> (pure (fromEnum s)))

instance Hashable AttachPolicy where

instance NFData AttachPolicy where

instance ToHeaders AttachPolicy where
        toHeaders AttachPolicy'{..}
          = mconcat ["x-amz-data-partition" =# _apDirectoryARN]

instance ToJSON AttachPolicy where
        toJSON AttachPolicy'{..}
          = object
              (catMaybes
                 [Just ("PolicyReference" .= _apPolicyReference),
                  Just ("ObjectReference" .= _apObjectReference)])

instance ToPath AttachPolicy where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/policy/attach"

instance ToQuery AttachPolicy where
        toQuery = const mempty

-- | /See:/ 'attachPolicyResponse' smart constructor.
newtype AttachPolicyResponse = AttachPolicyResponse'
  { _aprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aprsResponseStatus' - -- | The response status code.
attachPolicyResponse
    :: Int -- ^ 'aprsResponseStatus'
    -> AttachPolicyResponse
attachPolicyResponse pResponseStatus_ =
  AttachPolicyResponse' {_aprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
aprsResponseStatus :: Lens' AttachPolicyResponse Int
aprsResponseStatus = lens _aprsResponseStatus (\ s a -> s{_aprsResponseStatus = a})

instance NFData AttachPolicyResponse where

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
-- Module      : Network.AWS.WAFRegional.DeletePermissionPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes an IAM policy from the specified RuleGroup.
--
--
-- The user making the request must be the owner of the RuleGroup.
--
module Network.AWS.WAFRegional.DeletePermissionPolicy
    (
    -- * Creating a Request
      deletePermissionPolicy
    , DeletePermissionPolicy
    -- * Request Lenses
    , dppResourceARN

    -- * Destructuring the Response
    , deletePermissionPolicyResponse
    , DeletePermissionPolicyResponse
    -- * Response Lenses
    , dpprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'deletePermissionPolicy' smart constructor.
newtype DeletePermissionPolicy = DeletePermissionPolicy'
  { _dppResourceARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePermissionPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dppResourceARN' - The Amazon Resource Name (ARN) of the RuleGroup from which you want to delete the policy. The user making the request must be the owner of the RuleGroup.
deletePermissionPolicy
    :: Text -- ^ 'dppResourceARN'
    -> DeletePermissionPolicy
deletePermissionPolicy pResourceARN_ =
  DeletePermissionPolicy' {_dppResourceARN = pResourceARN_}


-- | The Amazon Resource Name (ARN) of the RuleGroup from which you want to delete the policy. The user making the request must be the owner of the RuleGroup.
dppResourceARN :: Lens' DeletePermissionPolicy Text
dppResourceARN = lens _dppResourceARN (\ s a -> s{_dppResourceARN = a})

instance AWSRequest DeletePermissionPolicy where
        type Rs DeletePermissionPolicy =
             DeletePermissionPolicyResponse
        request = postJSON wAFRegional
        response
          = receiveEmpty
              (\ s h x ->
                 DeletePermissionPolicyResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeletePermissionPolicy where

instance NFData DeletePermissionPolicy where

instance ToHeaders DeletePermissionPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.DeletePermissionPolicy" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeletePermissionPolicy where
        toJSON DeletePermissionPolicy'{..}
          = object
              (catMaybes [Just ("ResourceArn" .= _dppResourceARN)])

instance ToPath DeletePermissionPolicy where
        toPath = const "/"

instance ToQuery DeletePermissionPolicy where
        toQuery = const mempty

-- | /See:/ 'deletePermissionPolicyResponse' smart constructor.
newtype DeletePermissionPolicyResponse = DeletePermissionPolicyResponse'
  { _dpprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePermissionPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpprsResponseStatus' - -- | The response status code.
deletePermissionPolicyResponse
    :: Int -- ^ 'dpprsResponseStatus'
    -> DeletePermissionPolicyResponse
deletePermissionPolicyResponse pResponseStatus_ =
  DeletePermissionPolicyResponse' {_dpprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dpprsResponseStatus :: Lens' DeletePermissionPolicyResponse Int
dpprsResponseStatus = lens _dpprsResponseStatus (\ s a -> s{_dpprsResponseStatus = a})

instance NFData DeletePermissionPolicyResponse where

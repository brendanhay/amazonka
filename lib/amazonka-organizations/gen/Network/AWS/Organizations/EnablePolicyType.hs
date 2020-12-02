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
-- Module      : Network.AWS.Organizations.EnablePolicyType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a policy type in a root. After you enable a policy type in a root, you can attach policies of that type to the root, any OU, or account in that root. You can undo this by using the 'DisablePolicyType' operation.
--
--
-- This operation can be called only from the organization's master account.
--
-- You can enable a policy type in a root only if that policy type is available in the organization. Use 'DescribeOrganization' to view the status of available policy types in the organization.
--
-- To view the status of policy type in a root, use 'ListRoots' .
--
module Network.AWS.Organizations.EnablePolicyType
    (
    -- * Creating a Request
      enablePolicyType
    , EnablePolicyType
    -- * Request Lenses
    , eptRootId
    , eptPolicyType

    -- * Destructuring the Response
    , enablePolicyTypeResponse
    , EnablePolicyTypeResponse
    -- * Response Lenses
    , eptrsRoot
    , eptrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enablePolicyType' smart constructor.
data EnablePolicyType = EnablePolicyType'
  { _eptRootId     :: !Text
  , _eptPolicyType :: !PolicyType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnablePolicyType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eptRootId' - The unique identifier (ID) of the root in which you want to enable a policy type. You can get the ID from the 'ListRoots' operation. The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string requires "r-" followed by from 4 to 32 lower-case letters or digits.
--
-- * 'eptPolicyType' - The policy type that you want to enable.
enablePolicyType
    :: Text -- ^ 'eptRootId'
    -> PolicyType -- ^ 'eptPolicyType'
    -> EnablePolicyType
enablePolicyType pRootId_ pPolicyType_ =
  EnablePolicyType' {_eptRootId = pRootId_, _eptPolicyType = pPolicyType_}


-- | The unique identifier (ID) of the root in which you want to enable a policy type. You can get the ID from the 'ListRoots' operation. The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string requires "r-" followed by from 4 to 32 lower-case letters or digits.
eptRootId :: Lens' EnablePolicyType Text
eptRootId = lens _eptRootId (\ s a -> s{_eptRootId = a})

-- | The policy type that you want to enable.
eptPolicyType :: Lens' EnablePolicyType PolicyType
eptPolicyType = lens _eptPolicyType (\ s a -> s{_eptPolicyType = a})

instance AWSRequest EnablePolicyType where
        type Rs EnablePolicyType = EnablePolicyTypeResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 EnablePolicyTypeResponse' <$>
                   (x .?> "Root") <*> (pure (fromEnum s)))

instance Hashable EnablePolicyType where

instance NFData EnablePolicyType where

instance ToHeaders EnablePolicyType where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.EnablePolicyType" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON EnablePolicyType where
        toJSON EnablePolicyType'{..}
          = object
              (catMaybes
                 [Just ("RootId" .= _eptRootId),
                  Just ("PolicyType" .= _eptPolicyType)])

instance ToPath EnablePolicyType where
        toPath = const "/"

instance ToQuery EnablePolicyType where
        toQuery = const mempty

-- | /See:/ 'enablePolicyTypeResponse' smart constructor.
data EnablePolicyTypeResponse = EnablePolicyTypeResponse'
  { _eptrsRoot           :: !(Maybe Root)
  , _eptrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnablePolicyTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eptrsRoot' - A structure that shows the root with the updated list of enabled policy types.
--
-- * 'eptrsResponseStatus' - -- | The response status code.
enablePolicyTypeResponse
    :: Int -- ^ 'eptrsResponseStatus'
    -> EnablePolicyTypeResponse
enablePolicyTypeResponse pResponseStatus_ =
  EnablePolicyTypeResponse'
    {_eptrsRoot = Nothing, _eptrsResponseStatus = pResponseStatus_}


-- | A structure that shows the root with the updated list of enabled policy types.
eptrsRoot :: Lens' EnablePolicyTypeResponse (Maybe Root)
eptrsRoot = lens _eptrsRoot (\ s a -> s{_eptrsRoot = a})

-- | -- | The response status code.
eptrsResponseStatus :: Lens' EnablePolicyTypeResponse Int
eptrsResponseStatus = lens _eptrsResponseStatus (\ s a -> s{_eptrsResponseStatus = a})

instance NFData EnablePolicyTypeResponse where

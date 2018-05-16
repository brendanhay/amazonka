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
-- Module      : Network.AWS.Organizations.DisablePolicyType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables an organizational control policy type in a root. A policy of a certain type can be attached to entities in a root only if that type is enabled in the root. After you perform this operation, you no longer can attach policies of the specified type to that root or to any OU or account in that root. You can undo this by using the 'EnablePolicyType' operation.
--
--
-- This operation can be called only from the organization's master account.
--
module Network.AWS.Organizations.DisablePolicyType
    (
    -- * Creating a Request
      disablePolicyType
    , DisablePolicyType
    -- * Request Lenses
    , dptRootId
    , dptPolicyType

    -- * Destructuring the Response
    , disablePolicyTypeResponse
    , DisablePolicyTypeResponse
    -- * Response Lenses
    , dptrsRoot
    , dptrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disablePolicyType' smart constructor.
data DisablePolicyType = DisablePolicyType'
  { _dptRootId     :: !Text
  , _dptPolicyType :: !PolicyType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisablePolicyType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dptRootId' - The unique identifier (ID) of the root in which you want to disable a policy type. You can get the ID from the 'ListRoots' operation. The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string requires "r-" followed by from 4 to 32 lower-case letters or digits.
--
-- * 'dptPolicyType' - The policy type that you want to disable in this root.
disablePolicyType
    :: Text -- ^ 'dptRootId'
    -> PolicyType -- ^ 'dptPolicyType'
    -> DisablePolicyType
disablePolicyType pRootId_ pPolicyType_ =
  DisablePolicyType' {_dptRootId = pRootId_, _dptPolicyType = pPolicyType_}


-- | The unique identifier (ID) of the root in which you want to disable a policy type. You can get the ID from the 'ListRoots' operation. The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string requires "r-" followed by from 4 to 32 lower-case letters or digits.
dptRootId :: Lens' DisablePolicyType Text
dptRootId = lens _dptRootId (\ s a -> s{_dptRootId = a})

-- | The policy type that you want to disable in this root.
dptPolicyType :: Lens' DisablePolicyType PolicyType
dptPolicyType = lens _dptPolicyType (\ s a -> s{_dptPolicyType = a})

instance AWSRequest DisablePolicyType where
        type Rs DisablePolicyType = DisablePolicyTypeResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 DisablePolicyTypeResponse' <$>
                   (x .?> "Root") <*> (pure (fromEnum s)))

instance Hashable DisablePolicyType where

instance NFData DisablePolicyType where

instance ToHeaders DisablePolicyType where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.DisablePolicyType" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisablePolicyType where
        toJSON DisablePolicyType'{..}
          = object
              (catMaybes
                 [Just ("RootId" .= _dptRootId),
                  Just ("PolicyType" .= _dptPolicyType)])

instance ToPath DisablePolicyType where
        toPath = const "/"

instance ToQuery DisablePolicyType where
        toQuery = const mempty

-- | /See:/ 'disablePolicyTypeResponse' smart constructor.
data DisablePolicyTypeResponse = DisablePolicyTypeResponse'
  { _dptrsRoot           :: !(Maybe Root)
  , _dptrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisablePolicyTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dptrsRoot' - A structure that shows the root with the updated list of enabled policy types.
--
-- * 'dptrsResponseStatus' - -- | The response status code.
disablePolicyTypeResponse
    :: Int -- ^ 'dptrsResponseStatus'
    -> DisablePolicyTypeResponse
disablePolicyTypeResponse pResponseStatus_ =
  DisablePolicyTypeResponse'
    {_dptrsRoot = Nothing, _dptrsResponseStatus = pResponseStatus_}


-- | A structure that shows the root with the updated list of enabled policy types.
dptrsRoot :: Lens' DisablePolicyTypeResponse (Maybe Root)
dptrsRoot = lens _dptrsRoot (\ s a -> s{_dptrsRoot = a})

-- | -- | The response status code.
dptrsResponseStatus :: Lens' DisablePolicyTypeResponse Int
dptrsResponseStatus = lens _dptrsResponseStatus (\ s a -> s{_dptrsResponseStatus = a})

instance NFData DisablePolicyTypeResponse where

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
-- Module      : Network.AWS.Shield.DisassociateDRTRole
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the DDoS Response team's (DRT) access to your AWS account.
--
--
-- To make a @DisassociateDRTRole@ request, you must be subscribed to the <https://aws.amazon.com/premiumsupport/business-support/ Business Support plan> or the <https://aws.amazon.com/premiumsupport/enterprise-support/ Enterprise Support plan> . However, if you are not subscribed to one of these support plans, but had been previously and had granted the DRT access to your account, you can submit a @DisassociateDRTRole@ request to remove this access.
--
module Network.AWS.Shield.DisassociateDRTRole
    (
    -- * Creating a Request
      disassociateDRTRole
    , DisassociateDRTRole

    -- * Destructuring the Response
    , disassociateDRTRoleResponse
    , DisassociateDRTRoleResponse
    -- * Response Lenses
    , ddrtrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types
import Network.AWS.Shield.Types.Product

-- | /See:/ 'disassociateDRTRole' smart constructor.
data DisassociateDRTRole =
  DisassociateDRTRole'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateDRTRole' with the minimum fields required to make a request.
--
disassociateDRTRole
    :: DisassociateDRTRole
disassociateDRTRole = DisassociateDRTRole'


instance AWSRequest DisassociateDRTRole where
        type Rs DisassociateDRTRole =
             DisassociateDRTRoleResponse
        request = postJSON shield
        response
          = receiveEmpty
              (\ s h x ->
                 DisassociateDRTRoleResponse' <$> (pure (fromEnum s)))

instance Hashable DisassociateDRTRole where

instance NFData DisassociateDRTRole where

instance ToHeaders DisassociateDRTRole where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSShield_20160616.DisassociateDRTRole" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociateDRTRole where
        toJSON = const (Object mempty)

instance ToPath DisassociateDRTRole where
        toPath = const "/"

instance ToQuery DisassociateDRTRole where
        toQuery = const mempty

-- | /See:/ 'disassociateDRTRoleResponse' smart constructor.
newtype DisassociateDRTRoleResponse = DisassociateDRTRoleResponse'
  { _ddrtrrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateDRTRoleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrtrrsResponseStatus' - -- | The response status code.
disassociateDRTRoleResponse
    :: Int -- ^ 'ddrtrrsResponseStatus'
    -> DisassociateDRTRoleResponse
disassociateDRTRoleResponse pResponseStatus_ =
  DisassociateDRTRoleResponse' {_ddrtrrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ddrtrrsResponseStatus :: Lens' DisassociateDRTRoleResponse Int
ddrtrrsResponseStatus = lens _ddrtrrsResponseStatus (\ s a -> s{_ddrtrrsResponseStatus = a})

instance NFData DisassociateDRTRoleResponse where

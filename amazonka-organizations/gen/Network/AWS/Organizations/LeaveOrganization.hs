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
-- Module      : Network.AWS.Organizations.LeaveOrganization
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a member account from its parent organization. This version of the operation is performed by the account that wants to leave. To remove a member account as a user in the master account, use 'RemoveAccountFromOrganization' instead.
--
--
-- This operation can be called only from a member account in the organization.
--
-- /Important:/ The master account in an organization with all features enabled can set service control policies (SCPs) that can restrict what administrators of member accounts can do, including preventing them from successfully calling @LeaveOrganization@ and leaving the organization.
--
module Network.AWS.Organizations.LeaveOrganization
    (
    -- * Creating a Request
      leaveOrganization
    , LeaveOrganization

    -- * Destructuring the Response
    , leaveOrganizationResponse
    , LeaveOrganizationResponse
    ) where

import           Network.AWS.Lens
import           Network.AWS.Organizations.Types
import           Network.AWS.Organizations.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'leaveOrganization' smart constructor.
data LeaveOrganization =
    LeaveOrganization'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LeaveOrganization' with the minimum fields required to make a request.
--
leaveOrganization
    :: LeaveOrganization
leaveOrganization = LeaveOrganization'

instance AWSRequest LeaveOrganization where
        type Rs LeaveOrganization = LeaveOrganizationResponse
        request = postJSON organizations
        response = receiveNull LeaveOrganizationResponse'

instance Hashable LeaveOrganization

instance NFData LeaveOrganization

instance ToHeaders LeaveOrganization where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.LeaveOrganization" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON LeaveOrganization where
        toJSON = const (Object mempty)

instance ToPath LeaveOrganization where
        toPath = const "/"

instance ToQuery LeaveOrganization where
        toQuery = const mempty

-- | /See:/ 'leaveOrganizationResponse' smart constructor.
data LeaveOrganizationResponse =
    LeaveOrganizationResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LeaveOrganizationResponse' with the minimum fields required to make a request.
--
leaveOrganizationResponse
    :: LeaveOrganizationResponse
leaveOrganizationResponse = LeaveOrganizationResponse'

instance NFData LeaveOrganizationResponse

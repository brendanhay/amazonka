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
-- Module      : Network.AWS.StorageGateway.JoinDomain
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a file gateway to an Active Directory domain. This operation is only supported for file gateways that support the SMB file protocol.
--
--
module Network.AWS.StorageGateway.JoinDomain
    (
    -- * Creating a Request
      joinDomain
    , JoinDomain
    -- * Request Lenses
    , jdOrganizationalUnit
    , jdDomainControllers
    , jdGatewayARN
    , jdDomainName
    , jdUserName
    , jdPassword

    -- * Destructuring the Response
    , joinDomainResponse
    , JoinDomainResponse
    -- * Response Lenses
    , jdrsGatewayARN
    , jdrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | JoinDomainInput
--
--
--
-- /See:/ 'joinDomain' smart constructor.
data JoinDomain = JoinDomain'
  { _jdOrganizationalUnit :: !(Maybe Text)
  , _jdDomainControllers  :: !(Maybe [Text])
  , _jdGatewayARN         :: !Text
  , _jdDomainName         :: !Text
  , _jdUserName           :: !Text
  , _jdPassword           :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'JoinDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jdOrganizationalUnit' - The organizational unit (OU) is a container with an Active Directory that can hold users, groups, computers, and other OUs and this parameter specifies the OU that the gateway will join within the AD domain.
--
-- * 'jdDomainControllers' - List of IPv4 addresses, NetBIOS names, or host names of your domain server. If you need to specify the port number include it after the colon (

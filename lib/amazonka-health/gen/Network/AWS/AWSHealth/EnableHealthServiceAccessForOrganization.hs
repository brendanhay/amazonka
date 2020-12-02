{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.EnableHealthServiceAccessForOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Calling this operation enables AWS Health to work with AWS Organizations. This applies a service-linked role (SLR) to the master account in the organization. To call this operation, you must sign in as an IAM user, assume an IAM role, or sign in as the root user (not recommended) in the organization's master account.
--
--
-- For more information, see <https://docs.aws.amazon.com/health/latest/ug/aggregate-events.html Aggregating AWS Health events> in the /AWS Health User Guide/ .
module Network.AWS.AWSHealth.EnableHealthServiceAccessForOrganization
  ( -- * Creating a Request
    enableHealthServiceAccessForOrganization,
    EnableHealthServiceAccessForOrganization,

    -- * Destructuring the Response
    enableHealthServiceAccessForOrganizationResponse,
    EnableHealthServiceAccessForOrganizationResponse,
  )
where

import Network.AWS.AWSHealth.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enableHealthServiceAccessForOrganization' smart constructor.
data EnableHealthServiceAccessForOrganization = EnableHealthServiceAccessForOrganization'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnableHealthServiceAccessForOrganization' with the minimum fields required to make a request.
enableHealthServiceAccessForOrganization ::
  EnableHealthServiceAccessForOrganization
enableHealthServiceAccessForOrganization =
  EnableHealthServiceAccessForOrganization'

instance AWSRequest EnableHealthServiceAccessForOrganization where
  type
    Rs EnableHealthServiceAccessForOrganization =
      EnableHealthServiceAccessForOrganizationResponse
  request = postJSON awsHealth
  response =
    receiveNull EnableHealthServiceAccessForOrganizationResponse'

instance Hashable EnableHealthServiceAccessForOrganization

instance NFData EnableHealthServiceAccessForOrganization

instance ToHeaders EnableHealthServiceAccessForOrganization where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSHealth_20160804.EnableHealthServiceAccessForOrganization" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON EnableHealthServiceAccessForOrganization where
  toJSON = const (Object mempty)

instance ToPath EnableHealthServiceAccessForOrganization where
  toPath = const "/"

instance ToQuery EnableHealthServiceAccessForOrganization where
  toQuery = const mempty

-- | /See:/ 'enableHealthServiceAccessForOrganizationResponse' smart constructor.
data EnableHealthServiceAccessForOrganizationResponse = EnableHealthServiceAccessForOrganizationResponse'
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'EnableHealthServiceAccessForOrganizationResponse' with the minimum fields required to make a request.
enableHealthServiceAccessForOrganizationResponse ::
  EnableHealthServiceAccessForOrganizationResponse
enableHealthServiceAccessForOrganizationResponse =
  EnableHealthServiceAccessForOrganizationResponse'

instance NFData EnableHealthServiceAccessForOrganizationResponse

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
-- Module      : Network.AWS.AWSHealth.DisableHealthServiceAccessForOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables AWS Health from working with AWS Organizations. To call this operation, you must sign in as an AWS Identity and Access Management (IAM) user, assume an IAM role, or sign in as the root user (not recommended) in the organization's master AWS account. For more information, see <https://docs.aws.amazon.com/health/latest/ug/aggregate-events.html Aggregating AWS Health events> in the /AWS Health User Guide/ .
--
--
-- This operation doesn't remove the service-linked role (SLR) from the AWS master account in your organization. You must use the IAM console, API, or AWS Command Line Interface (AWS CLI) to remove the SLR. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/using-service-linked-roles.html#delete-service-linked-role Deleting a Service-Linked Role> in the /IAM User Guide/ .
module Network.AWS.AWSHealth.DisableHealthServiceAccessForOrganization
  ( -- * Creating a Request
    disableHealthServiceAccessForOrganization,
    DisableHealthServiceAccessForOrganization,

    -- * Destructuring the Response
    disableHealthServiceAccessForOrganizationResponse,
    DisableHealthServiceAccessForOrganizationResponse,
  )
where

import Network.AWS.AWSHealth.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disableHealthServiceAccessForOrganization' smart constructor.
data DisableHealthServiceAccessForOrganization = DisableHealthServiceAccessForOrganization'
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DisableHealthServiceAccessForOrganization' with the minimum fields required to make a request.
disableHealthServiceAccessForOrganization ::
  DisableHealthServiceAccessForOrganization
disableHealthServiceAccessForOrganization =
  DisableHealthServiceAccessForOrganization'

instance AWSRequest DisableHealthServiceAccessForOrganization where
  type
    Rs DisableHealthServiceAccessForOrganization =
      DisableHealthServiceAccessForOrganizationResponse
  request = postJSON awsHealth
  response =
    receiveNull DisableHealthServiceAccessForOrganizationResponse'

instance Hashable DisableHealthServiceAccessForOrganization

instance NFData DisableHealthServiceAccessForOrganization

instance ToHeaders DisableHealthServiceAccessForOrganization where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSHealth_20160804.DisableHealthServiceAccessForOrganization" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DisableHealthServiceAccessForOrganization where
  toJSON = const (Object mempty)

instance ToPath DisableHealthServiceAccessForOrganization where
  toPath = const "/"

instance ToQuery DisableHealthServiceAccessForOrganization where
  toQuery = const mempty

-- | /See:/ 'disableHealthServiceAccessForOrganizationResponse' smart constructor.
data DisableHealthServiceAccessForOrganizationResponse = DisableHealthServiceAccessForOrganizationResponse'
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DisableHealthServiceAccessForOrganizationResponse' with the minimum fields required to make a request.
disableHealthServiceAccessForOrganizationResponse ::
  DisableHealthServiceAccessForOrganizationResponse
disableHealthServiceAccessForOrganizationResponse =
  DisableHealthServiceAccessForOrganizationResponse'

instance NFData DisableHealthServiceAccessForOrganizationResponse

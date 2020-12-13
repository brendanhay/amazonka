{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- This operation doesn't remove the service-linked role (SLR) from the AWS master account in your organization. You must use the IAM console, API, or AWS Command Line Interface (AWS CLI) to remove the SLR. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/using-service-linked-roles.html#delete-service-linked-role Deleting a Service-Linked Role> in the /IAM User Guide/ .
module Network.AWS.AWSHealth.DisableHealthServiceAccessForOrganization
  ( -- * Creating a request
    DisableHealthServiceAccessForOrganization (..),
    mkDisableHealthServiceAccessForOrganization,

    -- * Destructuring the response
    DisableHealthServiceAccessForOrganizationResponse (..),
    mkDisableHealthServiceAccessForOrganizationResponse,
  )
where

import Network.AWS.AWSHealth.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisableHealthServiceAccessForOrganization' smart constructor.
data DisableHealthServiceAccessForOrganization = DisableHealthServiceAccessForOrganization'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableHealthServiceAccessForOrganization' with the minimum fields required to make a request.
mkDisableHealthServiceAccessForOrganization ::
  DisableHealthServiceAccessForOrganization
mkDisableHealthServiceAccessForOrganization =
  DisableHealthServiceAccessForOrganization'

instance Lude.AWSRequest DisableHealthServiceAccessForOrganization where
  type
    Rs DisableHealthServiceAccessForOrganization =
      DisableHealthServiceAccessForOrganizationResponse
  request = Req.postJSON awsHealthService
  response =
    Res.receiveNull
      DisableHealthServiceAccessForOrganizationResponse'

instance Lude.ToHeaders DisableHealthServiceAccessForOrganization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSHealth_20160804.DisableHealthServiceAccessForOrganization" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisableHealthServiceAccessForOrganization where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DisableHealthServiceAccessForOrganization where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableHealthServiceAccessForOrganization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisableHealthServiceAccessForOrganizationResponse' smart constructor.
data DisableHealthServiceAccessForOrganizationResponse = DisableHealthServiceAccessForOrganizationResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableHealthServiceAccessForOrganizationResponse' with the minimum fields required to make a request.
mkDisableHealthServiceAccessForOrganizationResponse ::
  DisableHealthServiceAccessForOrganizationResponse
mkDisableHealthServiceAccessForOrganizationResponse =
  DisableHealthServiceAccessForOrganizationResponse'

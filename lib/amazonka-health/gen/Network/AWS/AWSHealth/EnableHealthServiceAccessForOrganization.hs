{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- For more information, see <https://docs.aws.amazon.com/health/latest/ug/aggregate-events.html Aggregating AWS Health events> in the /AWS Health User Guide/ .
module Network.AWS.AWSHealth.EnableHealthServiceAccessForOrganization
  ( -- * Creating a request
    EnableHealthServiceAccessForOrganization (..),
    mkEnableHealthServiceAccessForOrganization,

    -- * Destructuring the response
    EnableHealthServiceAccessForOrganizationResponse (..),
    mkEnableHealthServiceAccessForOrganizationResponse,
  )
where

import Network.AWS.AWSHealth.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnableHealthServiceAccessForOrganization' smart constructor.
data EnableHealthServiceAccessForOrganization = EnableHealthServiceAccessForOrganization'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableHealthServiceAccessForOrganization' with the minimum fields required to make a request.
mkEnableHealthServiceAccessForOrganization ::
  EnableHealthServiceAccessForOrganization
mkEnableHealthServiceAccessForOrganization =
  EnableHealthServiceAccessForOrganization'

instance Lude.AWSRequest EnableHealthServiceAccessForOrganization where
  type
    Rs EnableHealthServiceAccessForOrganization =
      EnableHealthServiceAccessForOrganizationResponse
  request = Req.postJSON awsHealthService
  response =
    Res.receiveNull EnableHealthServiceAccessForOrganizationResponse'

instance Lude.ToHeaders EnableHealthServiceAccessForOrganization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSHealth_20160804.EnableHealthServiceAccessForOrganization" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON EnableHealthServiceAccessForOrganization where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath EnableHealthServiceAccessForOrganization where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableHealthServiceAccessForOrganization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkEnableHealthServiceAccessForOrganizationResponse' smart constructor.
data EnableHealthServiceAccessForOrganizationResponse = EnableHealthServiceAccessForOrganizationResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'EnableHealthServiceAccessForOrganizationResponse' with the minimum fields required to make a request.
mkEnableHealthServiceAccessForOrganizationResponse ::
  EnableHealthServiceAccessForOrganizationResponse
mkEnableHealthServiceAccessForOrganizationResponse =
  EnableHealthServiceAccessForOrganizationResponse'

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DeleteOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the organization. You can delete an organization only by using credentials from the management account. The organization must be empty of member accounts.
module Network.AWS.Organizations.DeleteOrganization
  ( -- * Creating a request
    DeleteOrganization (..),
    mkDeleteOrganization,

    -- * Destructuring the response
    DeleteOrganizationResponse (..),
    mkDeleteOrganizationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteOrganization' smart constructor.
data DeleteOrganization = DeleteOrganization'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteOrganization' with the minimum fields required to make a request.
mkDeleteOrganization ::
  DeleteOrganization
mkDeleteOrganization = DeleteOrganization'

instance Lude.AWSRequest DeleteOrganization where
  type Rs DeleteOrganization = DeleteOrganizationResponse
  request = Req.postJSON organizationsService
  response = Res.receiveNull DeleteOrganizationResponse'

instance Lude.ToHeaders DeleteOrganization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.DeleteOrganization" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteOrganization where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DeleteOrganization where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteOrganization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteOrganizationResponse' smart constructor.
data DeleteOrganizationResponse = DeleteOrganizationResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteOrganizationResponse' with the minimum fields required to make a request.
mkDeleteOrganizationResponse ::
  DeleteOrganizationResponse
mkDeleteOrganizationResponse = DeleteOrganizationResponse'

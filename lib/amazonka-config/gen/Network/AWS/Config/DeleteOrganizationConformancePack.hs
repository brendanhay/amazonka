{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteOrganizationConformancePack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified organization conformance pack and all of the config rules and remediation actions from all member accounts in that organization.
--
-- Only a master account or a delegated administrator account can delete an organization conformance pack. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
-- AWS Config sets the state of a conformance pack to DELETE_IN_PROGRESS until the deletion is complete. You cannot update a conformance pack while it is in this state.
module Network.AWS.Config.DeleteOrganizationConformancePack
  ( -- * Creating a request
    DeleteOrganizationConformancePack (..),
    mkDeleteOrganizationConformancePack,

    -- ** Request lenses
    docpOrganizationConformancePackName,

    -- * Destructuring the response
    DeleteOrganizationConformancePackResponse (..),
    mkDeleteOrganizationConformancePackResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteOrganizationConformancePack' smart constructor.
newtype DeleteOrganizationConformancePack = DeleteOrganizationConformancePack'
  { organizationConformancePackName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteOrganizationConformancePack' with the minimum fields required to make a request.
--
-- * 'organizationConformancePackName' - The name of organization conformance pack that you want to delete.
mkDeleteOrganizationConformancePack ::
  -- | 'organizationConformancePackName'
  Lude.Text ->
  DeleteOrganizationConformancePack
mkDeleteOrganizationConformancePack
  pOrganizationConformancePackName_ =
    DeleteOrganizationConformancePack'
      { organizationConformancePackName =
          pOrganizationConformancePackName_
      }

-- | The name of organization conformance pack that you want to delete.
--
-- /Note:/ Consider using 'organizationConformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docpOrganizationConformancePackName :: Lens.Lens' DeleteOrganizationConformancePack Lude.Text
docpOrganizationConformancePackName = Lens.lens (organizationConformancePackName :: DeleteOrganizationConformancePack -> Lude.Text) (\s a -> s {organizationConformancePackName = a} :: DeleteOrganizationConformancePack)
{-# DEPRECATED docpOrganizationConformancePackName "Use generic-lens or generic-optics with 'organizationConformancePackName' instead." #-}

instance Lude.AWSRequest DeleteOrganizationConformancePack where
  type
    Rs DeleteOrganizationConformancePack =
      DeleteOrganizationConformancePackResponse
  request = Req.postJSON configService
  response =
    Res.receiveNull DeleteOrganizationConformancePackResponse'

instance Lude.ToHeaders DeleteOrganizationConformancePack where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DeleteOrganizationConformancePack" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteOrganizationConformancePack where
  toJSON DeleteOrganizationConformancePack' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "OrganizationConformancePackName"
                  Lude..= organizationConformancePackName
              )
          ]
      )

instance Lude.ToPath DeleteOrganizationConformancePack where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteOrganizationConformancePack where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteOrganizationConformancePackResponse' smart constructor.
data DeleteOrganizationConformancePackResponse = DeleteOrganizationConformancePackResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteOrganizationConformancePackResponse' with the minimum fields required to make a request.
mkDeleteOrganizationConformancePackResponse ::
  DeleteOrganizationConformancePackResponse
mkDeleteOrganizationConformancePackResponse =
  DeleteOrganizationConformancePackResponse'

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DeleteOrganizationalUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an organizational unit (OU) from a root or another OU. You must first remove all accounts and child OUs from the OU that you want to delete.
--
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.DeleteOrganizationalUnit
  ( -- * Creating a request
    DeleteOrganizationalUnit (..),
    mkDeleteOrganizationalUnit,

    -- ** Request lenses
    dOrganizationalUnitId,

    -- * Destructuring the response
    DeleteOrganizationalUnitResponse (..),
    mkDeleteOrganizationalUnitResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteOrganizationalUnit' smart constructor.
newtype DeleteOrganizationalUnit = DeleteOrganizationalUnit'
  { organizationalUnitId ::
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

-- | Creates a value of 'DeleteOrganizationalUnit' with the minimum fields required to make a request.
--
-- * 'organizationalUnitId' - The unique identifier (ID) of the organizational unit that you want to delete. You can get the ID from the 'ListOrganizationalUnitsForParent' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
mkDeleteOrganizationalUnit ::
  -- | 'organizationalUnitId'
  Lude.Text ->
  DeleteOrganizationalUnit
mkDeleteOrganizationalUnit pOrganizationalUnitId_ =
  DeleteOrganizationalUnit'
    { organizationalUnitId =
        pOrganizationalUnitId_
    }

-- | The unique identifier (ID) of the organizational unit that you want to delete. You can get the ID from the 'ListOrganizationalUnitsForParent' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
-- /Note:/ Consider using 'organizationalUnitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOrganizationalUnitId :: Lens.Lens' DeleteOrganizationalUnit Lude.Text
dOrganizationalUnitId = Lens.lens (organizationalUnitId :: DeleteOrganizationalUnit -> Lude.Text) (\s a -> s {organizationalUnitId = a} :: DeleteOrganizationalUnit)
{-# DEPRECATED dOrganizationalUnitId "Use generic-lens or generic-optics with 'organizationalUnitId' instead." #-}

instance Lude.AWSRequest DeleteOrganizationalUnit where
  type Rs DeleteOrganizationalUnit = DeleteOrganizationalUnitResponse
  request = Req.postJSON organizationsService
  response = Res.receiveNull DeleteOrganizationalUnitResponse'

instance Lude.ToHeaders DeleteOrganizationalUnit where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.DeleteOrganizationalUnit" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteOrganizationalUnit where
  toJSON DeleteOrganizationalUnit' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("OrganizationalUnitId" Lude..= organizationalUnitId)]
      )

instance Lude.ToPath DeleteOrganizationalUnit where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteOrganizationalUnit where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteOrganizationalUnitResponse' smart constructor.
data DeleteOrganizationalUnitResponse = DeleteOrganizationalUnitResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteOrganizationalUnitResponse' with the minimum fields required to make a request.
mkDeleteOrganizationalUnitResponse ::
  DeleteOrganizationalUnitResponse
mkDeleteOrganizationalUnitResponse =
  DeleteOrganizationalUnitResponse'

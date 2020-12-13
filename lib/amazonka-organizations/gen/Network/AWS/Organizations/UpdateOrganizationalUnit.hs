{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.UpdateOrganizationalUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Renames the specified organizational unit (OU). The ID and ARN don't change. The child OUs and accounts remain in place, and any attached policies of the OU remain attached.
--
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.UpdateOrganizationalUnit
  ( -- * Creating a request
    UpdateOrganizationalUnit (..),
    mkUpdateOrganizationalUnit,

    -- ** Request lenses
    uouName,
    uouOrganizationalUnitId,

    -- * Destructuring the response
    UpdateOrganizationalUnitResponse (..),
    mkUpdateOrganizationalUnitResponse,

    -- ** Response lenses
    uoursOrganizationalUnit,
    uoursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateOrganizationalUnit' smart constructor.
data UpdateOrganizationalUnit = UpdateOrganizationalUnit'
  { -- | The new name that you want to assign to the OU.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
    name :: Lude.Maybe Lude.Text,
    -- | The unique identifier (ID) of the OU that you want to rename. You can get the ID from the 'ListOrganizationalUnitsForParent' operation.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
    organizationalUnitId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateOrganizationalUnit' with the minimum fields required to make a request.
--
-- * 'name' - The new name that you want to assign to the OU.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
-- * 'organizationalUnitId' - The unique identifier (ID) of the OU that you want to rename. You can get the ID from the 'ListOrganizationalUnitsForParent' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
mkUpdateOrganizationalUnit ::
  -- | 'organizationalUnitId'
  Lude.Text ->
  UpdateOrganizationalUnit
mkUpdateOrganizationalUnit pOrganizationalUnitId_ =
  UpdateOrganizationalUnit'
    { name = Lude.Nothing,
      organizationalUnitId = pOrganizationalUnitId_
    }

-- | The new name that you want to assign to the OU.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uouName :: Lens.Lens' UpdateOrganizationalUnit (Lude.Maybe Lude.Text)
uouName = Lens.lens (name :: UpdateOrganizationalUnit -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateOrganizationalUnit)
{-# DEPRECATED uouName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique identifier (ID) of the OU that you want to rename. You can get the ID from the 'ListOrganizationalUnitsForParent' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
-- /Note:/ Consider using 'organizationalUnitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uouOrganizationalUnitId :: Lens.Lens' UpdateOrganizationalUnit Lude.Text
uouOrganizationalUnitId = Lens.lens (organizationalUnitId :: UpdateOrganizationalUnit -> Lude.Text) (\s a -> s {organizationalUnitId = a} :: UpdateOrganizationalUnit)
{-# DEPRECATED uouOrganizationalUnitId "Use generic-lens or generic-optics with 'organizationalUnitId' instead." #-}

instance Lude.AWSRequest UpdateOrganizationalUnit where
  type Rs UpdateOrganizationalUnit = UpdateOrganizationalUnitResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateOrganizationalUnitResponse'
            Lude.<$> (x Lude..?> "OrganizationalUnit")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateOrganizationalUnit where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.UpdateOrganizationalUnit" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateOrganizationalUnit where
  toJSON UpdateOrganizationalUnit' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Name" Lude..=) Lude.<$> name,
            Lude.Just ("OrganizationalUnitId" Lude..= organizationalUnitId)
          ]
      )

instance Lude.ToPath UpdateOrganizationalUnit where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateOrganizationalUnit where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateOrganizationalUnitResponse' smart constructor.
data UpdateOrganizationalUnitResponse = UpdateOrganizationalUnitResponse'
  { -- | A structure that contains the details about the specified OU, including its new name.
    organizationalUnit :: Lude.Maybe OrganizationalUnit,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateOrganizationalUnitResponse' with the minimum fields required to make a request.
--
-- * 'organizationalUnit' - A structure that contains the details about the specified OU, including its new name.
-- * 'responseStatus' - The response status code.
mkUpdateOrganizationalUnitResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateOrganizationalUnitResponse
mkUpdateOrganizationalUnitResponse pResponseStatus_ =
  UpdateOrganizationalUnitResponse'
    { organizationalUnit =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure that contains the details about the specified OU, including its new name.
--
-- /Note:/ Consider using 'organizationalUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoursOrganizationalUnit :: Lens.Lens' UpdateOrganizationalUnitResponse (Lude.Maybe OrganizationalUnit)
uoursOrganizationalUnit = Lens.lens (organizationalUnit :: UpdateOrganizationalUnitResponse -> Lude.Maybe OrganizationalUnit) (\s a -> s {organizationalUnit = a} :: UpdateOrganizationalUnitResponse)
{-# DEPRECATED uoursOrganizationalUnit "Use generic-lens or generic-optics with 'organizationalUnit' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoursResponseStatus :: Lens.Lens' UpdateOrganizationalUnitResponse Lude.Int
uoursResponseStatus = Lens.lens (responseStatus :: UpdateOrganizationalUnitResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateOrganizationalUnitResponse)
{-# DEPRECATED uoursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

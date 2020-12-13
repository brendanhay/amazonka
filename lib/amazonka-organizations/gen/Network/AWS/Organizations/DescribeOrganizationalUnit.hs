{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DescribeOrganizationalUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an organizational unit (OU).
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
module Network.AWS.Organizations.DescribeOrganizationalUnit
  ( -- * Creating a request
    DescribeOrganizationalUnit (..),
    mkDescribeOrganizationalUnit,

    -- ** Request lenses
    douOrganizationalUnitId,

    -- * Destructuring the response
    DescribeOrganizationalUnitResponse (..),
    mkDescribeOrganizationalUnitResponse,

    -- ** Response lenses
    doursOrganizationalUnit,
    doursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeOrganizationalUnit' smart constructor.
newtype DescribeOrganizationalUnit = DescribeOrganizationalUnit'
  { -- | The unique identifier (ID) of the organizational unit that you want details about. You can get the ID from the 'ListOrganizationalUnitsForParent' operation.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
    organizationalUnitId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOrganizationalUnit' with the minimum fields required to make a request.
--
-- * 'organizationalUnitId' - The unique identifier (ID) of the organizational unit that you want details about. You can get the ID from the 'ListOrganizationalUnitsForParent' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
mkDescribeOrganizationalUnit ::
  -- | 'organizationalUnitId'
  Lude.Text ->
  DescribeOrganizationalUnit
mkDescribeOrganizationalUnit pOrganizationalUnitId_ =
  DescribeOrganizationalUnit'
    { organizationalUnitId =
        pOrganizationalUnitId_
    }

-- | The unique identifier (ID) of the organizational unit that you want details about. You can get the ID from the 'ListOrganizationalUnitsForParent' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
-- /Note:/ Consider using 'organizationalUnitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
douOrganizationalUnitId :: Lens.Lens' DescribeOrganizationalUnit Lude.Text
douOrganizationalUnitId = Lens.lens (organizationalUnitId :: DescribeOrganizationalUnit -> Lude.Text) (\s a -> s {organizationalUnitId = a} :: DescribeOrganizationalUnit)
{-# DEPRECATED douOrganizationalUnitId "Use generic-lens or generic-optics with 'organizationalUnitId' instead." #-}

instance Lude.AWSRequest DescribeOrganizationalUnit where
  type
    Rs DescribeOrganizationalUnit =
      DescribeOrganizationalUnitResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeOrganizationalUnitResponse'
            Lude.<$> (x Lude..?> "OrganizationalUnit")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeOrganizationalUnit where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.DescribeOrganizationalUnit" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeOrganizationalUnit where
  toJSON DescribeOrganizationalUnit' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("OrganizationalUnitId" Lude..= organizationalUnitId)]
      )

instance Lude.ToPath DescribeOrganizationalUnit where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeOrganizationalUnit where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeOrganizationalUnitResponse' smart constructor.
data DescribeOrganizationalUnitResponse = DescribeOrganizationalUnitResponse'
  { -- | A structure that contains details about the specified OU.
    organizationalUnit :: Lude.Maybe OrganizationalUnit,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOrganizationalUnitResponse' with the minimum fields required to make a request.
--
-- * 'organizationalUnit' - A structure that contains details about the specified OU.
-- * 'responseStatus' - The response status code.
mkDescribeOrganizationalUnitResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeOrganizationalUnitResponse
mkDescribeOrganizationalUnitResponse pResponseStatus_ =
  DescribeOrganizationalUnitResponse'
    { organizationalUnit =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure that contains details about the specified OU.
--
-- /Note:/ Consider using 'organizationalUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doursOrganizationalUnit :: Lens.Lens' DescribeOrganizationalUnitResponse (Lude.Maybe OrganizationalUnit)
doursOrganizationalUnit = Lens.lens (organizationalUnit :: DescribeOrganizationalUnitResponse -> Lude.Maybe OrganizationalUnit) (\s a -> s {organizationalUnit = a} :: DescribeOrganizationalUnitResponse)
{-# DEPRECATED doursOrganizationalUnit "Use generic-lens or generic-optics with 'organizationalUnit' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doursResponseStatus :: Lens.Lens' DescribeOrganizationalUnitResponse Lude.Int
doursResponseStatus = Lens.lens (responseStatus :: DescribeOrganizationalUnitResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeOrganizationalUnitResponse)
{-# DEPRECATED doursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

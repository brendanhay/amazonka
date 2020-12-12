{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncOrganizationalUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceDataSyncOrganizationalUnit
  ( ResourceDataSyncOrganizationalUnit (..),

    -- * Smart constructor
    mkResourceDataSyncOrganizationalUnit,

    -- * Lenses
    rdsouOrganizationalUnitId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The AWS Organizations organizational unit data source for the sync.
--
-- /See:/ 'mkResourceDataSyncOrganizationalUnit' smart constructor.
newtype ResourceDataSyncOrganizationalUnit = ResourceDataSyncOrganizationalUnit'
  { organizationalUnitId ::
      Lude.Maybe
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

-- | Creates a value of 'ResourceDataSyncOrganizationalUnit' with the minimum fields required to make a request.
--
-- * 'organizationalUnitId' - The AWS Organization unit ID data source for the sync.
mkResourceDataSyncOrganizationalUnit ::
  ResourceDataSyncOrganizationalUnit
mkResourceDataSyncOrganizationalUnit =
  ResourceDataSyncOrganizationalUnit'
    { organizationalUnitId =
        Lude.Nothing
    }

-- | The AWS Organization unit ID data source for the sync.
--
-- /Note:/ Consider using 'organizationalUnitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsouOrganizationalUnitId :: Lens.Lens' ResourceDataSyncOrganizationalUnit (Lude.Maybe Lude.Text)
rdsouOrganizationalUnitId = Lens.lens (organizationalUnitId :: ResourceDataSyncOrganizationalUnit -> Lude.Maybe Lude.Text) (\s a -> s {organizationalUnitId = a} :: ResourceDataSyncOrganizationalUnit)
{-# DEPRECATED rdsouOrganizationalUnitId "Use generic-lens or generic-optics with 'organizationalUnitId' instead." #-}

instance Lude.FromJSON ResourceDataSyncOrganizationalUnit where
  parseJSON =
    Lude.withObject
      "ResourceDataSyncOrganizationalUnit"
      ( \x ->
          ResourceDataSyncOrganizationalUnit'
            Lude.<$> (x Lude..:? "OrganizationalUnitId")
      )

instance Lude.ToJSON ResourceDataSyncOrganizationalUnit where
  toJSON ResourceDataSyncOrganizationalUnit' {..} =
    Lude.object
      ( Lude.catMaybes
          [("OrganizationalUnitId" Lude..=) Lude.<$> organizationalUnitId]
      )

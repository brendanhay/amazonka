{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncAWSOrganizationsSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceDataSyncAWSOrganizationsSource
  ( ResourceDataSyncAWSOrganizationsSource (..),

    -- * Smart constructor
    mkResourceDataSyncAWSOrganizationsSource,

    -- * Lenses
    rdsaosOrganizationalUnits,
    rdsaosOrganizationSourceType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.ResourceDataSyncOrganizationalUnit

-- | Information about the AwsOrganizationsSource resource data sync source. A sync source of this type can synchronize data from AWS Organizations or, if an AWS Organization is not present, from multiple AWS Regions.
--
-- /See:/ 'mkResourceDataSyncAWSOrganizationsSource' smart constructor.
data ResourceDataSyncAWSOrganizationsSource = ResourceDataSyncAWSOrganizationsSource'
  { organizationalUnits ::
      Lude.Maybe
        ( Lude.NonEmpty
            ResourceDataSyncOrganizationalUnit
        ),
    organizationSourceType ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceDataSyncAWSOrganizationsSource' with the minimum fields required to make a request.
--
-- * 'organizationSourceType' - If an AWS Organization is present, this is either @OrganizationalUnits@ or @EntireOrganization@ . For @OrganizationalUnits@ , the data is aggregated from a set of organization units. For @EntireOrganization@ , the data is aggregated from the entire AWS Organization.
-- * 'organizationalUnits' - The AWS Organizations organization units included in the sync.
mkResourceDataSyncAWSOrganizationsSource ::
  -- | 'organizationSourceType'
  Lude.Text ->
  ResourceDataSyncAWSOrganizationsSource
mkResourceDataSyncAWSOrganizationsSource pOrganizationSourceType_ =
  ResourceDataSyncAWSOrganizationsSource'
    { organizationalUnits =
        Lude.Nothing,
      organizationSourceType = pOrganizationSourceType_
    }

-- | The AWS Organizations organization units included in the sync.
--
-- /Note:/ Consider using 'organizationalUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsaosOrganizationalUnits :: Lens.Lens' ResourceDataSyncAWSOrganizationsSource (Lude.Maybe (Lude.NonEmpty ResourceDataSyncOrganizationalUnit))
rdsaosOrganizationalUnits = Lens.lens (organizationalUnits :: ResourceDataSyncAWSOrganizationsSource -> Lude.Maybe (Lude.NonEmpty ResourceDataSyncOrganizationalUnit)) (\s a -> s {organizationalUnits = a} :: ResourceDataSyncAWSOrganizationsSource)
{-# DEPRECATED rdsaosOrganizationalUnits "Use generic-lens or generic-optics with 'organizationalUnits' instead." #-}

-- | If an AWS Organization is present, this is either @OrganizationalUnits@ or @EntireOrganization@ . For @OrganizationalUnits@ , the data is aggregated from a set of organization units. For @EntireOrganization@ , the data is aggregated from the entire AWS Organization.
--
-- /Note:/ Consider using 'organizationSourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsaosOrganizationSourceType :: Lens.Lens' ResourceDataSyncAWSOrganizationsSource Lude.Text
rdsaosOrganizationSourceType = Lens.lens (organizationSourceType :: ResourceDataSyncAWSOrganizationsSource -> Lude.Text) (\s a -> s {organizationSourceType = a} :: ResourceDataSyncAWSOrganizationsSource)
{-# DEPRECATED rdsaosOrganizationSourceType "Use generic-lens or generic-optics with 'organizationSourceType' instead." #-}

instance Lude.FromJSON ResourceDataSyncAWSOrganizationsSource where
  parseJSON =
    Lude.withObject
      "ResourceDataSyncAWSOrganizationsSource"
      ( \x ->
          ResourceDataSyncAWSOrganizationsSource'
            Lude.<$> (x Lude..:? "OrganizationalUnits")
            Lude.<*> (x Lude..: "OrganizationSourceType")
      )

instance Lude.ToJSON ResourceDataSyncAWSOrganizationsSource where
  toJSON ResourceDataSyncAWSOrganizationsSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("OrganizationalUnits" Lude..=) Lude.<$> organizationalUnits,
            Lude.Just
              ("OrganizationSourceType" Lude..= organizationSourceType)
          ]
      )

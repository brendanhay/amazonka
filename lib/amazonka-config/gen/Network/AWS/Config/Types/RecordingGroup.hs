{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RecordingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RecordingGroup
  ( RecordingGroup (..),

    -- * Smart constructor
    mkRecordingGroup,

    -- * Lenses
    rgAllSupported,
    rgIncludeGlobalResourceTypes,
    rgResourceTypes,
  )
where

import Network.AWS.Config.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the types of AWS resource for which AWS Config records configuration changes.
--
-- In the recording group, you specify whether all supported types or specific types of resources are recorded.
-- By default, AWS Config records configuration changes for all supported types of regional resources that AWS Config discovers in the region in which it is running. Regional resources are tied to a region and can be used only in that region. Examples of regional resources are EC2 instances and EBS volumes.
-- You can also have AWS Config record configuration changes for supported types of global resources (for example, IAM resources). Global resources are not tied to an individual region and can be used in all regions.
-- /Important:/ The configuration details for any global resource are the same in all regions. If you customize AWS Config in multiple regions to record global resources, it will create multiple configuration items each time a global resource changes: one configuration item for each region. These configuration items will contain identical data. To prevent duplicate configuration items, you should consider customizing AWS Config in only one region to record global resources, unless you want the configuration items to be available in multiple regions.
-- If you don't want AWS Config to record all resources, you can specify which types of resources it will record with the @resourceTypes@ parameter.
-- For a list of supported resource types, see <https://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported Resource Types> .
-- For more information, see <https://docs.aws.amazon.com/config/latest/developerguide/select-resources.html Selecting Which Resources AWS Config Records> .
--
-- /See:/ 'mkRecordingGroup' smart constructor.
data RecordingGroup = RecordingGroup'
  { -- | Specifies whether AWS Config records configuration changes for every supported type of regional resource.
    --
    -- If you set this option to @true@ , when AWS Config adds support for a new type of regional resource, it starts recording resources of that type automatically.
    -- If you set this option to @true@ , you cannot enumerate a list of @resourceTypes@ .
    allSupported :: Lude.Maybe Lude.Bool,
    -- | Specifies whether AWS Config includes all supported types of global resources (for example, IAM resources) with the resources that it records.
    --
    -- Before you can set this option to @true@ , you must set the @allSupported@ option to @true@ .
    -- If you set this option to @true@ , when AWS Config adds support for a new type of global resource, it starts recording resources of that type automatically.
    -- The configuration details for any global resource are the same in all regions. To prevent duplicate configuration items, you should consider customizing AWS Config in only one region to record global resources.
    includeGlobalResourceTypes :: Lude.Maybe Lude.Bool,
    -- | A comma-separated list that specifies the types of AWS resources for which AWS Config records configuration changes (for example, @AWS::EC2::Instance@ or @AWS::CloudTrail::Trail@ ).
    --
    -- Before you can set this option to @true@ , you must set the @allSupported@ option to @false@ .
    -- If you set this option to @true@ , when AWS Config adds support for a new type of resource, it will not record resources of that type unless you manually add that type to your recording group.
    -- For a list of valid @resourceTypes@ values, see the __resourceType Value__ column in <https://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported AWS Resource Types> .
    resourceTypes :: Lude.Maybe [ResourceType]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecordingGroup' with the minimum fields required to make a request.
--
-- * 'allSupported' - Specifies whether AWS Config records configuration changes for every supported type of regional resource.
--
-- If you set this option to @true@ , when AWS Config adds support for a new type of regional resource, it starts recording resources of that type automatically.
-- If you set this option to @true@ , you cannot enumerate a list of @resourceTypes@ .
-- * 'includeGlobalResourceTypes' - Specifies whether AWS Config includes all supported types of global resources (for example, IAM resources) with the resources that it records.
--
-- Before you can set this option to @true@ , you must set the @allSupported@ option to @true@ .
-- If you set this option to @true@ , when AWS Config adds support for a new type of global resource, it starts recording resources of that type automatically.
-- The configuration details for any global resource are the same in all regions. To prevent duplicate configuration items, you should consider customizing AWS Config in only one region to record global resources.
-- * 'resourceTypes' - A comma-separated list that specifies the types of AWS resources for which AWS Config records configuration changes (for example, @AWS::EC2::Instance@ or @AWS::CloudTrail::Trail@ ).
--
-- Before you can set this option to @true@ , you must set the @allSupported@ option to @false@ .
-- If you set this option to @true@ , when AWS Config adds support for a new type of resource, it will not record resources of that type unless you manually add that type to your recording group.
-- For a list of valid @resourceTypes@ values, see the __resourceType Value__ column in <https://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported AWS Resource Types> .
mkRecordingGroup ::
  RecordingGroup
mkRecordingGroup =
  RecordingGroup'
    { allSupported = Lude.Nothing,
      includeGlobalResourceTypes = Lude.Nothing,
      resourceTypes = Lude.Nothing
    }

-- | Specifies whether AWS Config records configuration changes for every supported type of regional resource.
--
-- If you set this option to @true@ , when AWS Config adds support for a new type of regional resource, it starts recording resources of that type automatically.
-- If you set this option to @true@ , you cannot enumerate a list of @resourceTypes@ .
--
-- /Note:/ Consider using 'allSupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgAllSupported :: Lens.Lens' RecordingGroup (Lude.Maybe Lude.Bool)
rgAllSupported = Lens.lens (allSupported :: RecordingGroup -> Lude.Maybe Lude.Bool) (\s a -> s {allSupported = a} :: RecordingGroup)
{-# DEPRECATED rgAllSupported "Use generic-lens or generic-optics with 'allSupported' instead." #-}

-- | Specifies whether AWS Config includes all supported types of global resources (for example, IAM resources) with the resources that it records.
--
-- Before you can set this option to @true@ , you must set the @allSupported@ option to @true@ .
-- If you set this option to @true@ , when AWS Config adds support for a new type of global resource, it starts recording resources of that type automatically.
-- The configuration details for any global resource are the same in all regions. To prevent duplicate configuration items, you should consider customizing AWS Config in only one region to record global resources.
--
-- /Note:/ Consider using 'includeGlobalResourceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgIncludeGlobalResourceTypes :: Lens.Lens' RecordingGroup (Lude.Maybe Lude.Bool)
rgIncludeGlobalResourceTypes = Lens.lens (includeGlobalResourceTypes :: RecordingGroup -> Lude.Maybe Lude.Bool) (\s a -> s {includeGlobalResourceTypes = a} :: RecordingGroup)
{-# DEPRECATED rgIncludeGlobalResourceTypes "Use generic-lens or generic-optics with 'includeGlobalResourceTypes' instead." #-}

-- | A comma-separated list that specifies the types of AWS resources for which AWS Config records configuration changes (for example, @AWS::EC2::Instance@ or @AWS::CloudTrail::Trail@ ).
--
-- Before you can set this option to @true@ , you must set the @allSupported@ option to @false@ .
-- If you set this option to @true@ , when AWS Config adds support for a new type of resource, it will not record resources of that type unless you manually add that type to your recording group.
-- For a list of valid @resourceTypes@ values, see the __resourceType Value__ column in <https://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported AWS Resource Types> .
--
-- /Note:/ Consider using 'resourceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgResourceTypes :: Lens.Lens' RecordingGroup (Lude.Maybe [ResourceType])
rgResourceTypes = Lens.lens (resourceTypes :: RecordingGroup -> Lude.Maybe [ResourceType]) (\s a -> s {resourceTypes = a} :: RecordingGroup)
{-# DEPRECATED rgResourceTypes "Use generic-lens or generic-optics with 'resourceTypes' instead." #-}

instance Lude.FromJSON RecordingGroup where
  parseJSON =
    Lude.withObject
      "RecordingGroup"
      ( \x ->
          RecordingGroup'
            Lude.<$> (x Lude..:? "allSupported")
            Lude.<*> (x Lude..:? "includeGlobalResourceTypes")
            Lude.<*> (x Lude..:? "resourceTypes" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON RecordingGroup where
  toJSON RecordingGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("allSupported" Lude..=) Lude.<$> allSupported,
            ("includeGlobalResourceTypes" Lude..=)
              Lude.<$> includeGlobalResourceTypes,
            ("resourceTypes" Lude..=) Lude.<$> resourceTypes
          ]
      )

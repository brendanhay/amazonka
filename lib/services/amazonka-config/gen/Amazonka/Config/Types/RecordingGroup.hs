{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Config.Types.RecordingGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.RecordingGroup where

import Amazonka.Config.Types.ExclusionByResourceTypes
import Amazonka.Config.Types.RecordingStrategy
import Amazonka.Config.Types.ResourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies which resource types Config records for configuration changes.
-- In the recording group, you specify whether you want to record all
-- supported resource types or to include or exclude specific types of
-- resources.
--
-- By default, Config records configuration changes for all supported types
-- of /Regional resources/ that Config discovers in the Amazon Web Services
-- Region in which it is running. Regional resources are tied to a Region
-- and can be used only in that Region. Examples of Regional resources are
-- Amazon EC2 instances and Amazon EBS volumes.
--
-- You can also have Config record supported types of /global resources/.
-- Global resources are not tied to a specific Region and can be used in
-- all Regions. The global resource types that Config supports include IAM
-- users, groups, roles, and customer managed policies.
--
-- Global resource types onboarded to Config recording after February 2022
-- will be recorded only in the service\'s home Region for the commercial
-- partition and Amazon Web Services GovCloud (US-West) for the Amazon Web
-- Services GovCloud (US) partition. You can view the Configuration Items
-- for these new global resource types only in their home Region and Amazon
-- Web Services GovCloud (US-West).
--
-- If you don\'t want Config to record all resources, you can specify which
-- types of resources Config records with the @resourceTypes@ parameter.
--
-- For a list of supported resource types, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported Resource Types>
-- in the /Config developer guide/.
--
-- For more information and a table of the Home Regions for Global Resource
-- Types Onboarded after February 2022, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/select-resources.html Selecting Which Resources Config Records>
-- in the /Config developer guide/.
--
-- /See:/ 'newRecordingGroup' smart constructor.
data RecordingGroup = RecordingGroup'
  { -- | Specifies whether Config records configuration changes for all supported
    -- regional resource types.
    --
    -- If you set this field to @true@, when Config adds support for a new type
    -- of regional resource, Config starts recording resources of that type
    -- automatically.
    --
    -- If you set this field to @true@, you cannot enumerate specific resource
    -- types to record in the @resourceTypes@ field of
    -- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>,
    -- or to exclude in the @resourceTypes@ field of
    -- <https://docs.aws.amazon.com/config/latest/APIReference/API_ExclusionByResourceTypes.html ExclusionByResourceTypes>.
    allSupported :: Prelude.Maybe Prelude.Bool,
    -- | An object that specifies how Config excludes resource types from being
    -- recorded by the configuration recorder.
    --
    -- To use this option, you must set the @useOnly@ field of
    -- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingStrategy.html RecordingStrategy>
    -- to @EXCLUSION_BY_RESOURCE_TYPES@.
    exclusionByResourceTypes :: Prelude.Maybe ExclusionByResourceTypes,
    -- | Specifies whether Config records configuration changes for all supported
    -- global resources.
    --
    -- Before you set this field to @true@, set the @allSupported@ field of
    -- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>
    -- to @true@. Optionally, you can set the @useOnly@ field of
    -- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingStrategy.html RecordingStrategy>
    -- to @ALL_SUPPORTED_RESOURCE_TYPES@.
    --
    -- If you set this field to @true@, when Config adds support for a new type
    -- of global resource in the Region where you set up the configuration
    -- recorder, Config starts recording resources of that type automatically.
    --
    -- If you set this field to @false@ but list global resource types in the
    -- @resourceTypes@ field of
    -- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>,
    -- Config will still record configuration changes for those specified
    -- resource types /regardless/ of if you set the
    -- @includeGlobalResourceTypes@ field to false.
    --
    -- If you do not want to record configuration changes to global resource
    -- types, make sure to not list them in the @resourceTypes@ field in
    -- addition to setting the @includeGlobalResourceTypes@ field to false.
    includeGlobalResourceTypes :: Prelude.Maybe Prelude.Bool,
    -- | An object that specifies the recording strategy for the configuration
    -- recorder.
    --
    -- -   If you set the @useOnly@ field of
    --     <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingStrategy.html RecordingStrategy>
    --     to @ALL_SUPPORTED_RESOURCE_TYPES@, Config records configuration
    --     changes for all supported regional resource types. You also must set
    --     the @allSupported@ field of
    --     <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>
    --     to @true@. When Config adds support for a new type of regional
    --     resource, Config automatically starts recording resources of that
    --     type.
    --
    -- -   If you set the @useOnly@ field of
    --     <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingStrategy.html RecordingStrategy>
    --     to @INCLUSION_BY_RESOURCE_TYPES@, Config records configuration
    --     changes for only the resource types you specify in the
    --     @resourceTypes@ field of
    --     <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>.
    --
    -- -   If you set the @useOnly@ field of
    --     <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingStrategy.html RecordingStrategy>
    --     to @EXCLUSION_BY_RESOURCE_TYPES@, Config records configuration
    --     changes for all supported resource types except the resource types
    --     that you specify as exemptions to exclude from being recorded in the
    --     @resourceTypes@ field of
    --     <https://docs.aws.amazon.com/config/latest/APIReference/API_ExclusionByResourceTypes.html ExclusionByResourceTypes>.
    --
    -- The @recordingStrategy@ field is optional when you set the
    -- @allSupported@ field of
    -- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>
    -- to @true@.
    --
    -- The @recordingStrategy@ field is optional when you list resource types
    -- in the @resourceTypes@ field of
    -- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>.
    --
    -- The @recordingStrategy@ field is required if you list resource types to
    -- exclude from recording in the @resourceTypes@ field of
    -- <https://docs.aws.amazon.com/config/latest/APIReference/API_ExclusionByResourceTypes.html ExclusionByResourceTypes>.
    --
    -- If you choose @EXCLUSION_BY_RESOURCE_TYPES@ for the recording strategy,
    -- the @exclusionByResourceTypes@ field will override other properties in
    -- the request.
    --
    -- For example, even if you set @includeGlobalResourceTypes@ to false,
    -- global resource types will still be automatically recorded in this
    -- option unless those resource types are specifically listed as exemptions
    -- in the @resourceTypes@ field of @exclusionByResourceTypes@.
    --
    -- By default, if you choose the @EXCLUSION_BY_RESOURCE_TYPES@ recording
    -- strategy, when Config adds support for a new resource type in the Region
    -- where you set up the configuration recorder, including global resource
    -- types, Config starts recording resources of that type automatically.
    recordingStrategy :: Prelude.Maybe RecordingStrategy,
    -- | A comma-separated list that specifies which resource types Config
    -- records.
    --
    -- Optionally, you can set the @useOnly@ field of
    -- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingStrategy.html RecordingStrategy>
    -- to @INCLUSION_BY_RESOURCE_TYPES@.
    --
    -- To record all configuration changes, set the @allSupported@ field of
    -- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>
    -- to @true@, and either omit this field or don\'t specify any resource
    -- types in this field. If you set the @allSupported@ field to @false@ and
    -- specify values for @resourceTypes@, when Config adds support for a new
    -- type of resource, it will not record resources of that type unless you
    -- manually add that type to your recording group.
    --
    -- For a list of valid @resourceTypes@ values, see the __Resource Type
    -- Value__ column in
    -- <https://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported Amazon Web Services resource Types>
    -- in the /Config developer guide/.
    --
    -- __Region Availability__
    --
    -- Before specifying a resource type for Config to track, check
    -- <https://docs.aws.amazon.com/config/latest/developerguide/what-is-resource-config-coverage.html Resource Coverage by Region Availability>
    -- to see if the resource type is supported in the Amazon Web Services
    -- Region where you set up Config. If a resource type is supported by
    -- Config in at least one Region, you can enable the recording of that
    -- resource type in all Regions supported by Config, even if the specified
    -- resource type is not supported in the Amazon Web Services Region where
    -- you set up Config.
    resourceTypes :: Prelude.Maybe [ResourceType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecordingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allSupported', 'recordingGroup_allSupported' - Specifies whether Config records configuration changes for all supported
-- regional resource types.
--
-- If you set this field to @true@, when Config adds support for a new type
-- of regional resource, Config starts recording resources of that type
-- automatically.
--
-- If you set this field to @true@, you cannot enumerate specific resource
-- types to record in the @resourceTypes@ field of
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>,
-- or to exclude in the @resourceTypes@ field of
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_ExclusionByResourceTypes.html ExclusionByResourceTypes>.
--
-- 'exclusionByResourceTypes', 'recordingGroup_exclusionByResourceTypes' - An object that specifies how Config excludes resource types from being
-- recorded by the configuration recorder.
--
-- To use this option, you must set the @useOnly@ field of
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingStrategy.html RecordingStrategy>
-- to @EXCLUSION_BY_RESOURCE_TYPES@.
--
-- 'includeGlobalResourceTypes', 'recordingGroup_includeGlobalResourceTypes' - Specifies whether Config records configuration changes for all supported
-- global resources.
--
-- Before you set this field to @true@, set the @allSupported@ field of
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>
-- to @true@. Optionally, you can set the @useOnly@ field of
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingStrategy.html RecordingStrategy>
-- to @ALL_SUPPORTED_RESOURCE_TYPES@.
--
-- If you set this field to @true@, when Config adds support for a new type
-- of global resource in the Region where you set up the configuration
-- recorder, Config starts recording resources of that type automatically.
--
-- If you set this field to @false@ but list global resource types in the
-- @resourceTypes@ field of
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>,
-- Config will still record configuration changes for those specified
-- resource types /regardless/ of if you set the
-- @includeGlobalResourceTypes@ field to false.
--
-- If you do not want to record configuration changes to global resource
-- types, make sure to not list them in the @resourceTypes@ field in
-- addition to setting the @includeGlobalResourceTypes@ field to false.
--
-- 'recordingStrategy', 'recordingGroup_recordingStrategy' - An object that specifies the recording strategy for the configuration
-- recorder.
--
-- -   If you set the @useOnly@ field of
--     <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingStrategy.html RecordingStrategy>
--     to @ALL_SUPPORTED_RESOURCE_TYPES@, Config records configuration
--     changes for all supported regional resource types. You also must set
--     the @allSupported@ field of
--     <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>
--     to @true@. When Config adds support for a new type of regional
--     resource, Config automatically starts recording resources of that
--     type.
--
-- -   If you set the @useOnly@ field of
--     <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingStrategy.html RecordingStrategy>
--     to @INCLUSION_BY_RESOURCE_TYPES@, Config records configuration
--     changes for only the resource types you specify in the
--     @resourceTypes@ field of
--     <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>.
--
-- -   If you set the @useOnly@ field of
--     <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingStrategy.html RecordingStrategy>
--     to @EXCLUSION_BY_RESOURCE_TYPES@, Config records configuration
--     changes for all supported resource types except the resource types
--     that you specify as exemptions to exclude from being recorded in the
--     @resourceTypes@ field of
--     <https://docs.aws.amazon.com/config/latest/APIReference/API_ExclusionByResourceTypes.html ExclusionByResourceTypes>.
--
-- The @recordingStrategy@ field is optional when you set the
-- @allSupported@ field of
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>
-- to @true@.
--
-- The @recordingStrategy@ field is optional when you list resource types
-- in the @resourceTypes@ field of
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>.
--
-- The @recordingStrategy@ field is required if you list resource types to
-- exclude from recording in the @resourceTypes@ field of
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_ExclusionByResourceTypes.html ExclusionByResourceTypes>.
--
-- If you choose @EXCLUSION_BY_RESOURCE_TYPES@ for the recording strategy,
-- the @exclusionByResourceTypes@ field will override other properties in
-- the request.
--
-- For example, even if you set @includeGlobalResourceTypes@ to false,
-- global resource types will still be automatically recorded in this
-- option unless those resource types are specifically listed as exemptions
-- in the @resourceTypes@ field of @exclusionByResourceTypes@.
--
-- By default, if you choose the @EXCLUSION_BY_RESOURCE_TYPES@ recording
-- strategy, when Config adds support for a new resource type in the Region
-- where you set up the configuration recorder, including global resource
-- types, Config starts recording resources of that type automatically.
--
-- 'resourceTypes', 'recordingGroup_resourceTypes' - A comma-separated list that specifies which resource types Config
-- records.
--
-- Optionally, you can set the @useOnly@ field of
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingStrategy.html RecordingStrategy>
-- to @INCLUSION_BY_RESOURCE_TYPES@.
--
-- To record all configuration changes, set the @allSupported@ field of
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>
-- to @true@, and either omit this field or don\'t specify any resource
-- types in this field. If you set the @allSupported@ field to @false@ and
-- specify values for @resourceTypes@, when Config adds support for a new
-- type of resource, it will not record resources of that type unless you
-- manually add that type to your recording group.
--
-- For a list of valid @resourceTypes@ values, see the __Resource Type
-- Value__ column in
-- <https://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported Amazon Web Services resource Types>
-- in the /Config developer guide/.
--
-- __Region Availability__
--
-- Before specifying a resource type for Config to track, check
-- <https://docs.aws.amazon.com/config/latest/developerguide/what-is-resource-config-coverage.html Resource Coverage by Region Availability>
-- to see if the resource type is supported in the Amazon Web Services
-- Region where you set up Config. If a resource type is supported by
-- Config in at least one Region, you can enable the recording of that
-- resource type in all Regions supported by Config, even if the specified
-- resource type is not supported in the Amazon Web Services Region where
-- you set up Config.
newRecordingGroup ::
  RecordingGroup
newRecordingGroup =
  RecordingGroup'
    { allSupported = Prelude.Nothing,
      exclusionByResourceTypes = Prelude.Nothing,
      includeGlobalResourceTypes = Prelude.Nothing,
      recordingStrategy = Prelude.Nothing,
      resourceTypes = Prelude.Nothing
    }

-- | Specifies whether Config records configuration changes for all supported
-- regional resource types.
--
-- If you set this field to @true@, when Config adds support for a new type
-- of regional resource, Config starts recording resources of that type
-- automatically.
--
-- If you set this field to @true@, you cannot enumerate specific resource
-- types to record in the @resourceTypes@ field of
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>,
-- or to exclude in the @resourceTypes@ field of
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_ExclusionByResourceTypes.html ExclusionByResourceTypes>.
recordingGroup_allSupported :: Lens.Lens' RecordingGroup (Prelude.Maybe Prelude.Bool)
recordingGroup_allSupported = Lens.lens (\RecordingGroup' {allSupported} -> allSupported) (\s@RecordingGroup' {} a -> s {allSupported = a} :: RecordingGroup)

-- | An object that specifies how Config excludes resource types from being
-- recorded by the configuration recorder.
--
-- To use this option, you must set the @useOnly@ field of
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingStrategy.html RecordingStrategy>
-- to @EXCLUSION_BY_RESOURCE_TYPES@.
recordingGroup_exclusionByResourceTypes :: Lens.Lens' RecordingGroup (Prelude.Maybe ExclusionByResourceTypes)
recordingGroup_exclusionByResourceTypes = Lens.lens (\RecordingGroup' {exclusionByResourceTypes} -> exclusionByResourceTypes) (\s@RecordingGroup' {} a -> s {exclusionByResourceTypes = a} :: RecordingGroup)

-- | Specifies whether Config records configuration changes for all supported
-- global resources.
--
-- Before you set this field to @true@, set the @allSupported@ field of
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>
-- to @true@. Optionally, you can set the @useOnly@ field of
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingStrategy.html RecordingStrategy>
-- to @ALL_SUPPORTED_RESOURCE_TYPES@.
--
-- If you set this field to @true@, when Config adds support for a new type
-- of global resource in the Region where you set up the configuration
-- recorder, Config starts recording resources of that type automatically.
--
-- If you set this field to @false@ but list global resource types in the
-- @resourceTypes@ field of
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>,
-- Config will still record configuration changes for those specified
-- resource types /regardless/ of if you set the
-- @includeGlobalResourceTypes@ field to false.
--
-- If you do not want to record configuration changes to global resource
-- types, make sure to not list them in the @resourceTypes@ field in
-- addition to setting the @includeGlobalResourceTypes@ field to false.
recordingGroup_includeGlobalResourceTypes :: Lens.Lens' RecordingGroup (Prelude.Maybe Prelude.Bool)
recordingGroup_includeGlobalResourceTypes = Lens.lens (\RecordingGroup' {includeGlobalResourceTypes} -> includeGlobalResourceTypes) (\s@RecordingGroup' {} a -> s {includeGlobalResourceTypes = a} :: RecordingGroup)

-- | An object that specifies the recording strategy for the configuration
-- recorder.
--
-- -   If you set the @useOnly@ field of
--     <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingStrategy.html RecordingStrategy>
--     to @ALL_SUPPORTED_RESOURCE_TYPES@, Config records configuration
--     changes for all supported regional resource types. You also must set
--     the @allSupported@ field of
--     <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>
--     to @true@. When Config adds support for a new type of regional
--     resource, Config automatically starts recording resources of that
--     type.
--
-- -   If you set the @useOnly@ field of
--     <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingStrategy.html RecordingStrategy>
--     to @INCLUSION_BY_RESOURCE_TYPES@, Config records configuration
--     changes for only the resource types you specify in the
--     @resourceTypes@ field of
--     <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>.
--
-- -   If you set the @useOnly@ field of
--     <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingStrategy.html RecordingStrategy>
--     to @EXCLUSION_BY_RESOURCE_TYPES@, Config records configuration
--     changes for all supported resource types except the resource types
--     that you specify as exemptions to exclude from being recorded in the
--     @resourceTypes@ field of
--     <https://docs.aws.amazon.com/config/latest/APIReference/API_ExclusionByResourceTypes.html ExclusionByResourceTypes>.
--
-- The @recordingStrategy@ field is optional when you set the
-- @allSupported@ field of
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>
-- to @true@.
--
-- The @recordingStrategy@ field is optional when you list resource types
-- in the @resourceTypes@ field of
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>.
--
-- The @recordingStrategy@ field is required if you list resource types to
-- exclude from recording in the @resourceTypes@ field of
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_ExclusionByResourceTypes.html ExclusionByResourceTypes>.
--
-- If you choose @EXCLUSION_BY_RESOURCE_TYPES@ for the recording strategy,
-- the @exclusionByResourceTypes@ field will override other properties in
-- the request.
--
-- For example, even if you set @includeGlobalResourceTypes@ to false,
-- global resource types will still be automatically recorded in this
-- option unless those resource types are specifically listed as exemptions
-- in the @resourceTypes@ field of @exclusionByResourceTypes@.
--
-- By default, if you choose the @EXCLUSION_BY_RESOURCE_TYPES@ recording
-- strategy, when Config adds support for a new resource type in the Region
-- where you set up the configuration recorder, including global resource
-- types, Config starts recording resources of that type automatically.
recordingGroup_recordingStrategy :: Lens.Lens' RecordingGroup (Prelude.Maybe RecordingStrategy)
recordingGroup_recordingStrategy = Lens.lens (\RecordingGroup' {recordingStrategy} -> recordingStrategy) (\s@RecordingGroup' {} a -> s {recordingStrategy = a} :: RecordingGroup)

-- | A comma-separated list that specifies which resource types Config
-- records.
--
-- Optionally, you can set the @useOnly@ field of
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingStrategy.html RecordingStrategy>
-- to @INCLUSION_BY_RESOURCE_TYPES@.
--
-- To record all configuration changes, set the @allSupported@ field of
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>
-- to @true@, and either omit this field or don\'t specify any resource
-- types in this field. If you set the @allSupported@ field to @false@ and
-- specify values for @resourceTypes@, when Config adds support for a new
-- type of resource, it will not record resources of that type unless you
-- manually add that type to your recording group.
--
-- For a list of valid @resourceTypes@ values, see the __Resource Type
-- Value__ column in
-- <https://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported Amazon Web Services resource Types>
-- in the /Config developer guide/.
--
-- __Region Availability__
--
-- Before specifying a resource type for Config to track, check
-- <https://docs.aws.amazon.com/config/latest/developerguide/what-is-resource-config-coverage.html Resource Coverage by Region Availability>
-- to see if the resource type is supported in the Amazon Web Services
-- Region where you set up Config. If a resource type is supported by
-- Config in at least one Region, you can enable the recording of that
-- resource type in all Regions supported by Config, even if the specified
-- resource type is not supported in the Amazon Web Services Region where
-- you set up Config.
recordingGroup_resourceTypes :: Lens.Lens' RecordingGroup (Prelude.Maybe [ResourceType])
recordingGroup_resourceTypes = Lens.lens (\RecordingGroup' {resourceTypes} -> resourceTypes) (\s@RecordingGroup' {} a -> s {resourceTypes = a} :: RecordingGroup) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RecordingGroup where
  parseJSON =
    Data.withObject
      "RecordingGroup"
      ( \x ->
          RecordingGroup'
            Prelude.<$> (x Data..:? "allSupported")
            Prelude.<*> (x Data..:? "exclusionByResourceTypes")
            Prelude.<*> (x Data..:? "includeGlobalResourceTypes")
            Prelude.<*> (x Data..:? "recordingStrategy")
            Prelude.<*> (x Data..:? "resourceTypes" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable RecordingGroup where
  hashWithSalt _salt RecordingGroup' {..} =
    _salt
      `Prelude.hashWithSalt` allSupported
      `Prelude.hashWithSalt` exclusionByResourceTypes
      `Prelude.hashWithSalt` includeGlobalResourceTypes
      `Prelude.hashWithSalt` recordingStrategy
      `Prelude.hashWithSalt` resourceTypes

instance Prelude.NFData RecordingGroup where
  rnf RecordingGroup' {..} =
    Prelude.rnf allSupported
      `Prelude.seq` Prelude.rnf exclusionByResourceTypes
      `Prelude.seq` Prelude.rnf includeGlobalResourceTypes
      `Prelude.seq` Prelude.rnf recordingStrategy
      `Prelude.seq` Prelude.rnf resourceTypes

instance Data.ToJSON RecordingGroup where
  toJSON RecordingGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("allSupported" Data..=) Prelude.<$> allSupported,
            ("exclusionByResourceTypes" Data..=)
              Prelude.<$> exclusionByResourceTypes,
            ("includeGlobalResourceTypes" Data..=)
              Prelude.<$> includeGlobalResourceTypes,
            ("recordingStrategy" Data..=)
              Prelude.<$> recordingStrategy,
            ("resourceTypes" Data..=) Prelude.<$> resourceTypes
          ]
      )

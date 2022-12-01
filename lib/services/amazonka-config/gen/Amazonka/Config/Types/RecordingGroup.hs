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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.RecordingGroup where

import Amazonka.Config.Types.ResourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the types of Amazon Web Services resource for which Config
-- records configuration changes.
--
-- In the recording group, you specify whether all supported types or
-- specific types of resources are recorded.
--
-- By default, Config records configuration changes for all supported types
-- of regional resources that Config discovers in the region in which it is
-- running. Regional resources are tied to a region and can be used only in
-- that region. Examples of regional resources are EC2 instances and EBS
-- volumes.
--
-- You can also have Config record configuration changes for supported
-- types of global resources (for example, IAM resources). Global resources
-- are not tied to an individual region and can be used in all regions.
--
-- The configuration details for any global resource are the same in all
-- regions. If you customize Config in multiple regions to record global
-- resources, it will create multiple configuration items each time a
-- global resource changes: one configuration item for each region. These
-- configuration items will contain identical data. To prevent duplicate
-- configuration items, you should consider customizing Config in only one
-- region to record global resources, unless you want the configuration
-- items to be available in multiple regions.
--
-- If you don\'t want Config to record all resources, you can specify which
-- types of resources it will record with the @resourceTypes@ parameter.
--
-- For a list of supported resource types, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported Resource Types>.
--
-- For more information, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/select-resources.html Selecting Which Resources Config Records>.
--
-- /See:/ 'newRecordingGroup' smart constructor.
data RecordingGroup = RecordingGroup'
  { -- | Specifies whether Config includes all supported types of global
    -- resources (for example, IAM resources) with the resources that it
    -- records.
    --
    -- Before you can set this option to @true@, you must set the
    -- @allSupported@ option to @true@.
    --
    -- If you set this option to @true@, when Config adds support for a new
    -- type of global resource, it starts recording resources of that type
    -- automatically.
    --
    -- The configuration details for any global resource are the same in all
    -- regions. To prevent duplicate configuration items, you should consider
    -- customizing Config in only one region to record global resources.
    includeGlobalResourceTypes :: Prelude.Maybe Prelude.Bool,
    -- | A comma-separated list that specifies the types of Amazon Web Services
    -- resources for which Config records configuration changes (for example,
    -- @AWS::EC2::Instance@ or @AWS::CloudTrail::Trail@).
    --
    -- To record all configuration changes, you must set the @allSupported@
    -- option to @true@.
    --
    -- If you set this option to @false@, when Config adds support for a new
    -- type of resource, it will not record resources of that type unless you
    -- manually add that type to your recording group.
    --
    -- For a list of valid @resourceTypes@ values, see the __resourceType
    -- Value__ column in
    -- <https://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported Amazon Web Services resource Types>.
    resourceTypes :: Prelude.Maybe [ResourceType],
    -- | Specifies whether Config records configuration changes for every
    -- supported type of regional resource.
    --
    -- If you set this option to @true@, when Config adds support for a new
    -- type of regional resource, it starts recording resources of that type
    -- automatically.
    --
    -- If you set this option to @true@, you cannot enumerate a list of
    -- @resourceTypes@.
    allSupported :: Prelude.Maybe Prelude.Bool
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
-- 'includeGlobalResourceTypes', 'recordingGroup_includeGlobalResourceTypes' - Specifies whether Config includes all supported types of global
-- resources (for example, IAM resources) with the resources that it
-- records.
--
-- Before you can set this option to @true@, you must set the
-- @allSupported@ option to @true@.
--
-- If you set this option to @true@, when Config adds support for a new
-- type of global resource, it starts recording resources of that type
-- automatically.
--
-- The configuration details for any global resource are the same in all
-- regions. To prevent duplicate configuration items, you should consider
-- customizing Config in only one region to record global resources.
--
-- 'resourceTypes', 'recordingGroup_resourceTypes' - A comma-separated list that specifies the types of Amazon Web Services
-- resources for which Config records configuration changes (for example,
-- @AWS::EC2::Instance@ or @AWS::CloudTrail::Trail@).
--
-- To record all configuration changes, you must set the @allSupported@
-- option to @true@.
--
-- If you set this option to @false@, when Config adds support for a new
-- type of resource, it will not record resources of that type unless you
-- manually add that type to your recording group.
--
-- For a list of valid @resourceTypes@ values, see the __resourceType
-- Value__ column in
-- <https://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported Amazon Web Services resource Types>.
--
-- 'allSupported', 'recordingGroup_allSupported' - Specifies whether Config records configuration changes for every
-- supported type of regional resource.
--
-- If you set this option to @true@, when Config adds support for a new
-- type of regional resource, it starts recording resources of that type
-- automatically.
--
-- If you set this option to @true@, you cannot enumerate a list of
-- @resourceTypes@.
newRecordingGroup ::
  RecordingGroup
newRecordingGroup =
  RecordingGroup'
    { includeGlobalResourceTypes =
        Prelude.Nothing,
      resourceTypes = Prelude.Nothing,
      allSupported = Prelude.Nothing
    }

-- | Specifies whether Config includes all supported types of global
-- resources (for example, IAM resources) with the resources that it
-- records.
--
-- Before you can set this option to @true@, you must set the
-- @allSupported@ option to @true@.
--
-- If you set this option to @true@, when Config adds support for a new
-- type of global resource, it starts recording resources of that type
-- automatically.
--
-- The configuration details for any global resource are the same in all
-- regions. To prevent duplicate configuration items, you should consider
-- customizing Config in only one region to record global resources.
recordingGroup_includeGlobalResourceTypes :: Lens.Lens' RecordingGroup (Prelude.Maybe Prelude.Bool)
recordingGroup_includeGlobalResourceTypes = Lens.lens (\RecordingGroup' {includeGlobalResourceTypes} -> includeGlobalResourceTypes) (\s@RecordingGroup' {} a -> s {includeGlobalResourceTypes = a} :: RecordingGroup)

-- | A comma-separated list that specifies the types of Amazon Web Services
-- resources for which Config records configuration changes (for example,
-- @AWS::EC2::Instance@ or @AWS::CloudTrail::Trail@).
--
-- To record all configuration changes, you must set the @allSupported@
-- option to @true@.
--
-- If you set this option to @false@, when Config adds support for a new
-- type of resource, it will not record resources of that type unless you
-- manually add that type to your recording group.
--
-- For a list of valid @resourceTypes@ values, see the __resourceType
-- Value__ column in
-- <https://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported Amazon Web Services resource Types>.
recordingGroup_resourceTypes :: Lens.Lens' RecordingGroup (Prelude.Maybe [ResourceType])
recordingGroup_resourceTypes = Lens.lens (\RecordingGroup' {resourceTypes} -> resourceTypes) (\s@RecordingGroup' {} a -> s {resourceTypes = a} :: RecordingGroup) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether Config records configuration changes for every
-- supported type of regional resource.
--
-- If you set this option to @true@, when Config adds support for a new
-- type of regional resource, it starts recording resources of that type
-- automatically.
--
-- If you set this option to @true@, you cannot enumerate a list of
-- @resourceTypes@.
recordingGroup_allSupported :: Lens.Lens' RecordingGroup (Prelude.Maybe Prelude.Bool)
recordingGroup_allSupported = Lens.lens (\RecordingGroup' {allSupported} -> allSupported) (\s@RecordingGroup' {} a -> s {allSupported = a} :: RecordingGroup)

instance Core.FromJSON RecordingGroup where
  parseJSON =
    Core.withObject
      "RecordingGroup"
      ( \x ->
          RecordingGroup'
            Prelude.<$> (x Core..:? "includeGlobalResourceTypes")
            Prelude.<*> (x Core..:? "resourceTypes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "allSupported")
      )

instance Prelude.Hashable RecordingGroup where
  hashWithSalt _salt RecordingGroup' {..} =
    _salt
      `Prelude.hashWithSalt` includeGlobalResourceTypes
      `Prelude.hashWithSalt` resourceTypes
      `Prelude.hashWithSalt` allSupported

instance Prelude.NFData RecordingGroup where
  rnf RecordingGroup' {..} =
    Prelude.rnf includeGlobalResourceTypes
      `Prelude.seq` Prelude.rnf resourceTypes
      `Prelude.seq` Prelude.rnf allSupported

instance Core.ToJSON RecordingGroup where
  toJSON RecordingGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("includeGlobalResourceTypes" Core..=)
              Prelude.<$> includeGlobalResourceTypes,
            ("resourceTypes" Core..=) Prelude.<$> resourceTypes,
            ("allSupported" Core..=) Prelude.<$> allSupported
          ]
      )

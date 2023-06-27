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
-- Module      : Amazonka.Config.Types.RecordingStrategy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.RecordingStrategy where

import Amazonka.Config.Types.RecordingStrategyType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the recording strategy of the configuration recorder.
--
-- /See:/ 'newRecordingStrategy' smart constructor.
data RecordingStrategy = RecordingStrategy'
  { -- | The recording strategy for the configuration recorder.
    --
    -- -   If you set this option to @ALL_SUPPORTED_RESOURCE_TYPES@, Config
    --     records configuration changes for all supported regional resource
    --     types. You also must set the @allSupported@ field of
    --     <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>
    --     to @true@.
    --
    --     When Config adds support for a new type of regional resource, Config
    --     automatically starts recording resources of that type. For a list of
    --     supported resource types, see
    --     <https://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported Resource Types>
    --     in the /Config developer guide/.
    --
    -- -   If you set this option to @INCLUSION_BY_RESOURCE_TYPES@, Config
    --     records configuration changes for only the resource types that you
    --     specify in the @resourceTypes@ field of
    --     <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>.
    --
    -- -   If you set this option to @EXCLUSION_BY_RESOURCE_TYPES@, Config
    --     records configuration changes for all supported resource types,
    --     except the resource types that you specify as exemptions to exclude
    --     from being recorded in the @resourceTypes@ field of
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
    useOnly :: Prelude.Maybe RecordingStrategyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecordingStrategy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'useOnly', 'recordingStrategy_useOnly' - The recording strategy for the configuration recorder.
--
-- -   If you set this option to @ALL_SUPPORTED_RESOURCE_TYPES@, Config
--     records configuration changes for all supported regional resource
--     types. You also must set the @allSupported@ field of
--     <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>
--     to @true@.
--
--     When Config adds support for a new type of regional resource, Config
--     automatically starts recording resources of that type. For a list of
--     supported resource types, see
--     <https://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported Resource Types>
--     in the /Config developer guide/.
--
-- -   If you set this option to @INCLUSION_BY_RESOURCE_TYPES@, Config
--     records configuration changes for only the resource types that you
--     specify in the @resourceTypes@ field of
--     <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>.
--
-- -   If you set this option to @EXCLUSION_BY_RESOURCE_TYPES@, Config
--     records configuration changes for all supported resource types,
--     except the resource types that you specify as exemptions to exclude
--     from being recorded in the @resourceTypes@ field of
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
newRecordingStrategy ::
  RecordingStrategy
newRecordingStrategy =
  RecordingStrategy' {useOnly = Prelude.Nothing}

-- | The recording strategy for the configuration recorder.
--
-- -   If you set this option to @ALL_SUPPORTED_RESOURCE_TYPES@, Config
--     records configuration changes for all supported regional resource
--     types. You also must set the @allSupported@ field of
--     <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>
--     to @true@.
--
--     When Config adds support for a new type of regional resource, Config
--     automatically starts recording resources of that type. For a list of
--     supported resource types, see
--     <https://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported Resource Types>
--     in the /Config developer guide/.
--
-- -   If you set this option to @INCLUSION_BY_RESOURCE_TYPES@, Config
--     records configuration changes for only the resource types that you
--     specify in the @resourceTypes@ field of
--     <https://docs.aws.amazon.com/config/latest/APIReference/API_RecordingGroup.html RecordingGroup>.
--
-- -   If you set this option to @EXCLUSION_BY_RESOURCE_TYPES@, Config
--     records configuration changes for all supported resource types,
--     except the resource types that you specify as exemptions to exclude
--     from being recorded in the @resourceTypes@ field of
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
recordingStrategy_useOnly :: Lens.Lens' RecordingStrategy (Prelude.Maybe RecordingStrategyType)
recordingStrategy_useOnly = Lens.lens (\RecordingStrategy' {useOnly} -> useOnly) (\s@RecordingStrategy' {} a -> s {useOnly = a} :: RecordingStrategy)

instance Data.FromJSON RecordingStrategy where
  parseJSON =
    Data.withObject
      "RecordingStrategy"
      ( \x ->
          RecordingStrategy'
            Prelude.<$> (x Data..:? "useOnly")
      )

instance Prelude.Hashable RecordingStrategy where
  hashWithSalt _salt RecordingStrategy' {..} =
    _salt `Prelude.hashWithSalt` useOnly

instance Prelude.NFData RecordingStrategy where
  rnf RecordingStrategy' {..} = Prelude.rnf useOnly

instance Data.ToJSON RecordingStrategy where
  toJSON RecordingStrategy' {..} =
    Data.object
      ( Prelude.catMaybes
          [("useOnly" Data..=) Prelude.<$> useOnly]
      )

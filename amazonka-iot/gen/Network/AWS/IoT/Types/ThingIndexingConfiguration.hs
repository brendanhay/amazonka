{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.Types.ThingIndexingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingIndexingConfiguration where

import Network.AWS.IoT.Types.Field
import Network.AWS.IoT.Types.ThingConnectivityIndexingMode
import Network.AWS.IoT.Types.ThingIndexingMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The thing indexing configuration. For more information, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/managing-index.html Managing Thing Indexing>.
--
-- /See:/ 'newThingIndexingConfiguration' smart constructor.
data ThingIndexingConfiguration = ThingIndexingConfiguration'
  { -- | Contains fields that are indexed and whose types are already known by
    -- the Fleet Indexing service.
    managedFields :: Prelude.Maybe [Field],
    -- | Thing connectivity indexing mode. Valid values are:
    --
    -- -   STATUS – Your thing index contains connectivity status. To enable
    --     thing connectivity indexing, thingIndexMode must not be set to OFF.
    --
    -- -   OFF - Thing connectivity status indexing is disabled.
    thingConnectivityIndexingMode :: Prelude.Maybe ThingConnectivityIndexingMode,
    -- | Contains custom field names and their data type.
    customFields :: Prelude.Maybe [Field],
    -- | Thing indexing mode. Valid values are:
    --
    -- -   REGISTRY – Your thing index contains registry data only.
    --
    -- -   REGISTRY_AND_SHADOW - Your thing index contains registry and shadow
    --     data.
    --
    -- -   OFF - Thing indexing is disabled.
    thingIndexingMode :: ThingIndexingMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ThingIndexingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managedFields', 'thingIndexingConfiguration_managedFields' - Contains fields that are indexed and whose types are already known by
-- the Fleet Indexing service.
--
-- 'thingConnectivityIndexingMode', 'thingIndexingConfiguration_thingConnectivityIndexingMode' - Thing connectivity indexing mode. Valid values are:
--
-- -   STATUS – Your thing index contains connectivity status. To enable
--     thing connectivity indexing, thingIndexMode must not be set to OFF.
--
-- -   OFF - Thing connectivity status indexing is disabled.
--
-- 'customFields', 'thingIndexingConfiguration_customFields' - Contains custom field names and their data type.
--
-- 'thingIndexingMode', 'thingIndexingConfiguration_thingIndexingMode' - Thing indexing mode. Valid values are:
--
-- -   REGISTRY – Your thing index contains registry data only.
--
-- -   REGISTRY_AND_SHADOW - Your thing index contains registry and shadow
--     data.
--
-- -   OFF - Thing indexing is disabled.
newThingIndexingConfiguration ::
  -- | 'thingIndexingMode'
  ThingIndexingMode ->
  ThingIndexingConfiguration
newThingIndexingConfiguration pThingIndexingMode_ =
  ThingIndexingConfiguration'
    { managedFields =
        Prelude.Nothing,
      thingConnectivityIndexingMode = Prelude.Nothing,
      customFields = Prelude.Nothing,
      thingIndexingMode = pThingIndexingMode_
    }

-- | Contains fields that are indexed and whose types are already known by
-- the Fleet Indexing service.
thingIndexingConfiguration_managedFields :: Lens.Lens' ThingIndexingConfiguration (Prelude.Maybe [Field])
thingIndexingConfiguration_managedFields = Lens.lens (\ThingIndexingConfiguration' {managedFields} -> managedFields) (\s@ThingIndexingConfiguration' {} a -> s {managedFields = a} :: ThingIndexingConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | Thing connectivity indexing mode. Valid values are:
--
-- -   STATUS – Your thing index contains connectivity status. To enable
--     thing connectivity indexing, thingIndexMode must not be set to OFF.
--
-- -   OFF - Thing connectivity status indexing is disabled.
thingIndexingConfiguration_thingConnectivityIndexingMode :: Lens.Lens' ThingIndexingConfiguration (Prelude.Maybe ThingConnectivityIndexingMode)
thingIndexingConfiguration_thingConnectivityIndexingMode = Lens.lens (\ThingIndexingConfiguration' {thingConnectivityIndexingMode} -> thingConnectivityIndexingMode) (\s@ThingIndexingConfiguration' {} a -> s {thingConnectivityIndexingMode = a} :: ThingIndexingConfiguration)

-- | Contains custom field names and their data type.
thingIndexingConfiguration_customFields :: Lens.Lens' ThingIndexingConfiguration (Prelude.Maybe [Field])
thingIndexingConfiguration_customFields = Lens.lens (\ThingIndexingConfiguration' {customFields} -> customFields) (\s@ThingIndexingConfiguration' {} a -> s {customFields = a} :: ThingIndexingConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | Thing indexing mode. Valid values are:
--
-- -   REGISTRY – Your thing index contains registry data only.
--
-- -   REGISTRY_AND_SHADOW - Your thing index contains registry and shadow
--     data.
--
-- -   OFF - Thing indexing is disabled.
thingIndexingConfiguration_thingIndexingMode :: Lens.Lens' ThingIndexingConfiguration ThingIndexingMode
thingIndexingConfiguration_thingIndexingMode = Lens.lens (\ThingIndexingConfiguration' {thingIndexingMode} -> thingIndexingMode) (\s@ThingIndexingConfiguration' {} a -> s {thingIndexingMode = a} :: ThingIndexingConfiguration)

instance Prelude.FromJSON ThingIndexingConfiguration where
  parseJSON =
    Prelude.withObject
      "ThingIndexingConfiguration"
      ( \x ->
          ThingIndexingConfiguration'
            Prelude.<$> ( x Prelude..:? "managedFields"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "thingConnectivityIndexingMode")
            Prelude.<*> ( x Prelude..:? "customFields"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "thingIndexingMode")
      )

instance Prelude.Hashable ThingIndexingConfiguration

instance Prelude.NFData ThingIndexingConfiguration

instance Prelude.ToJSON ThingIndexingConfiguration where
  toJSON ThingIndexingConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("managedFields" Prelude..=)
              Prelude.<$> managedFields,
            ("thingConnectivityIndexingMode" Prelude..=)
              Prelude.<$> thingConnectivityIndexingMode,
            ("customFields" Prelude..=) Prelude.<$> customFields,
            Prelude.Just
              ("thingIndexingMode" Prelude..= thingIndexingMode)
          ]
      )

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
-- Module      : Amazonka.IoTSiteWise.Types.Attribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.Attribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains an asset attribute property. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-properties.html#attributes Attributes>
-- in the /IoT SiteWise User Guide/.
--
-- /See:/ 'newAttribute' smart constructor.
data Attribute = Attribute'
  { -- | The default value of the asset model property attribute. All assets that
    -- you create from the asset model contain this attribute value. You can
    -- update an attribute\'s value after you create an asset. For more
    -- information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/update-attribute-values.html Updating attribute values>
    -- in the /IoT SiteWise User Guide/.
    defaultValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Attribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValue', 'attribute_defaultValue' - The default value of the asset model property attribute. All assets that
-- you create from the asset model contain this attribute value. You can
-- update an attribute\'s value after you create an asset. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/update-attribute-values.html Updating attribute values>
-- in the /IoT SiteWise User Guide/.
newAttribute ::
  Attribute
newAttribute =
  Attribute' {defaultValue = Prelude.Nothing}

-- | The default value of the asset model property attribute. All assets that
-- you create from the asset model contain this attribute value. You can
-- update an attribute\'s value after you create an asset. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/update-attribute-values.html Updating attribute values>
-- in the /IoT SiteWise User Guide/.
attribute_defaultValue :: Lens.Lens' Attribute (Prelude.Maybe Prelude.Text)
attribute_defaultValue = Lens.lens (\Attribute' {defaultValue} -> defaultValue) (\s@Attribute' {} a -> s {defaultValue = a} :: Attribute)

instance Data.FromJSON Attribute where
  parseJSON =
    Data.withObject
      "Attribute"
      ( \x ->
          Attribute' Prelude.<$> (x Data..:? "defaultValue")
      )

instance Prelude.Hashable Attribute where
  hashWithSalt _salt Attribute' {..} =
    _salt `Prelude.hashWithSalt` defaultValue

instance Prelude.NFData Attribute where
  rnf Attribute' {..} = Prelude.rnf defaultValue

instance Data.ToJSON Attribute where
  toJSON Attribute' {..} =
    Data.object
      ( Prelude.catMaybes
          [("defaultValue" Data..=) Prelude.<$> defaultValue]
      )

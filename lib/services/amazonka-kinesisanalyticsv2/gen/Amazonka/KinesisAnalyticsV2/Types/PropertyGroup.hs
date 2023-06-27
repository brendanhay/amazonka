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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.PropertyGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.PropertyGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Property key-value pairs passed into an application.
--
-- /See:/ 'newPropertyGroup' smart constructor.
data PropertyGroup = PropertyGroup'
  { -- | Describes the key of an application execution property key-value pair.
    propertyGroupId :: Prelude.Text,
    -- | Describes the value of an application execution property key-value pair.
    propertyMap :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PropertyGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'propertyGroupId', 'propertyGroup_propertyGroupId' - Describes the key of an application execution property key-value pair.
--
-- 'propertyMap', 'propertyGroup_propertyMap' - Describes the value of an application execution property key-value pair.
newPropertyGroup ::
  -- | 'propertyGroupId'
  Prelude.Text ->
  PropertyGroup
newPropertyGroup pPropertyGroupId_ =
  PropertyGroup'
    { propertyGroupId = pPropertyGroupId_,
      propertyMap = Prelude.mempty
    }

-- | Describes the key of an application execution property key-value pair.
propertyGroup_propertyGroupId :: Lens.Lens' PropertyGroup Prelude.Text
propertyGroup_propertyGroupId = Lens.lens (\PropertyGroup' {propertyGroupId} -> propertyGroupId) (\s@PropertyGroup' {} a -> s {propertyGroupId = a} :: PropertyGroup)

-- | Describes the value of an application execution property key-value pair.
propertyGroup_propertyMap :: Lens.Lens' PropertyGroup (Prelude.HashMap Prelude.Text Prelude.Text)
propertyGroup_propertyMap = Lens.lens (\PropertyGroup' {propertyMap} -> propertyMap) (\s@PropertyGroup' {} a -> s {propertyMap = a} :: PropertyGroup) Prelude.. Lens.coerced

instance Data.FromJSON PropertyGroup where
  parseJSON =
    Data.withObject
      "PropertyGroup"
      ( \x ->
          PropertyGroup'
            Prelude.<$> (x Data..: "PropertyGroupId")
            Prelude.<*> (x Data..:? "PropertyMap" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable PropertyGroup where
  hashWithSalt _salt PropertyGroup' {..} =
    _salt
      `Prelude.hashWithSalt` propertyGroupId
      `Prelude.hashWithSalt` propertyMap

instance Prelude.NFData PropertyGroup where
  rnf PropertyGroup' {..} =
    Prelude.rnf propertyGroupId
      `Prelude.seq` Prelude.rnf propertyMap

instance Data.ToJSON PropertyGroup where
  toJSON PropertyGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PropertyGroupId" Data..= propertyGroupId),
            Prelude.Just ("PropertyMap" Data..= propertyMap)
          ]
      )

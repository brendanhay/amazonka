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
-- Module      : Network.AWS.IoT.Types.ThingAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingAttribute where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The properties of the thing, including thing name, thing type name, and
-- a list of thing attributes.
--
-- /See:/ 'newThingAttribute' smart constructor.
data ThingAttribute = ThingAttribute'
  { -- | The thing ARN.
    thingArn :: Core.Maybe Core.Text,
    -- | The name of the thing.
    thingName :: Core.Maybe Core.Text,
    -- | The version of the thing record in the registry.
    version :: Core.Maybe Core.Integer,
    -- | A list of thing attributes which are name-value pairs.
    attributes :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The name of the thing type, if the thing has been associated with a
    -- type.
    thingTypeName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ThingAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingArn', 'thingAttribute_thingArn' - The thing ARN.
--
-- 'thingName', 'thingAttribute_thingName' - The name of the thing.
--
-- 'version', 'thingAttribute_version' - The version of the thing record in the registry.
--
-- 'attributes', 'thingAttribute_attributes' - A list of thing attributes which are name-value pairs.
--
-- 'thingTypeName', 'thingAttribute_thingTypeName' - The name of the thing type, if the thing has been associated with a
-- type.
newThingAttribute ::
  ThingAttribute
newThingAttribute =
  ThingAttribute'
    { thingArn = Core.Nothing,
      thingName = Core.Nothing,
      version = Core.Nothing,
      attributes = Core.Nothing,
      thingTypeName = Core.Nothing
    }

-- | The thing ARN.
thingAttribute_thingArn :: Lens.Lens' ThingAttribute (Core.Maybe Core.Text)
thingAttribute_thingArn = Lens.lens (\ThingAttribute' {thingArn} -> thingArn) (\s@ThingAttribute' {} a -> s {thingArn = a} :: ThingAttribute)

-- | The name of the thing.
thingAttribute_thingName :: Lens.Lens' ThingAttribute (Core.Maybe Core.Text)
thingAttribute_thingName = Lens.lens (\ThingAttribute' {thingName} -> thingName) (\s@ThingAttribute' {} a -> s {thingName = a} :: ThingAttribute)

-- | The version of the thing record in the registry.
thingAttribute_version :: Lens.Lens' ThingAttribute (Core.Maybe Core.Integer)
thingAttribute_version = Lens.lens (\ThingAttribute' {version} -> version) (\s@ThingAttribute' {} a -> s {version = a} :: ThingAttribute)

-- | A list of thing attributes which are name-value pairs.
thingAttribute_attributes :: Lens.Lens' ThingAttribute (Core.Maybe (Core.HashMap Core.Text Core.Text))
thingAttribute_attributes = Lens.lens (\ThingAttribute' {attributes} -> attributes) (\s@ThingAttribute' {} a -> s {attributes = a} :: ThingAttribute) Core.. Lens.mapping Lens._Coerce

-- | The name of the thing type, if the thing has been associated with a
-- type.
thingAttribute_thingTypeName :: Lens.Lens' ThingAttribute (Core.Maybe Core.Text)
thingAttribute_thingTypeName = Lens.lens (\ThingAttribute' {thingTypeName} -> thingTypeName) (\s@ThingAttribute' {} a -> s {thingTypeName = a} :: ThingAttribute)

instance Core.FromJSON ThingAttribute where
  parseJSON =
    Core.withObject
      "ThingAttribute"
      ( \x ->
          ThingAttribute'
            Core.<$> (x Core..:? "thingArn")
            Core.<*> (x Core..:? "thingName")
            Core.<*> (x Core..:? "version")
            Core.<*> (x Core..:? "attributes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "thingTypeName")
      )

instance Core.Hashable ThingAttribute

instance Core.NFData ThingAttribute

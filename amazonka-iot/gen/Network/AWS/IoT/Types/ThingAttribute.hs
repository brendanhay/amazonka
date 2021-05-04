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
-- Module      : Network.AWS.IoT.Types.ThingAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingAttribute where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The properties of the thing, including thing name, thing type name, and
-- a list of thing attributes.
--
-- /See:/ 'newThingAttribute' smart constructor.
data ThingAttribute = ThingAttribute'
  { -- | The thing ARN.
    thingArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The version of the thing record in the registry.
    version :: Prelude.Maybe Prelude.Integer,
    -- | A list of thing attributes which are name-value pairs.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the thing type, if the thing has been associated with a
    -- type.
    thingTypeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { thingArn = Prelude.Nothing,
      thingName = Prelude.Nothing,
      version = Prelude.Nothing,
      attributes = Prelude.Nothing,
      thingTypeName = Prelude.Nothing
    }

-- | The thing ARN.
thingAttribute_thingArn :: Lens.Lens' ThingAttribute (Prelude.Maybe Prelude.Text)
thingAttribute_thingArn = Lens.lens (\ThingAttribute' {thingArn} -> thingArn) (\s@ThingAttribute' {} a -> s {thingArn = a} :: ThingAttribute)

-- | The name of the thing.
thingAttribute_thingName :: Lens.Lens' ThingAttribute (Prelude.Maybe Prelude.Text)
thingAttribute_thingName = Lens.lens (\ThingAttribute' {thingName} -> thingName) (\s@ThingAttribute' {} a -> s {thingName = a} :: ThingAttribute)

-- | The version of the thing record in the registry.
thingAttribute_version :: Lens.Lens' ThingAttribute (Prelude.Maybe Prelude.Integer)
thingAttribute_version = Lens.lens (\ThingAttribute' {version} -> version) (\s@ThingAttribute' {} a -> s {version = a} :: ThingAttribute)

-- | A list of thing attributes which are name-value pairs.
thingAttribute_attributes :: Lens.Lens' ThingAttribute (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
thingAttribute_attributes = Lens.lens (\ThingAttribute' {attributes} -> attributes) (\s@ThingAttribute' {} a -> s {attributes = a} :: ThingAttribute) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the thing type, if the thing has been associated with a
-- type.
thingAttribute_thingTypeName :: Lens.Lens' ThingAttribute (Prelude.Maybe Prelude.Text)
thingAttribute_thingTypeName = Lens.lens (\ThingAttribute' {thingTypeName} -> thingTypeName) (\s@ThingAttribute' {} a -> s {thingTypeName = a} :: ThingAttribute)

instance Prelude.FromJSON ThingAttribute where
  parseJSON =
    Prelude.withObject
      "ThingAttribute"
      ( \x ->
          ThingAttribute'
            Prelude.<$> (x Prelude..:? "thingArn")
            Prelude.<*> (x Prelude..:? "thingName")
            Prelude.<*> (x Prelude..:? "version")
            Prelude.<*> ( x Prelude..:? "attributes"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "thingTypeName")
      )

instance Prelude.Hashable ThingAttribute

instance Prelude.NFData ThingAttribute

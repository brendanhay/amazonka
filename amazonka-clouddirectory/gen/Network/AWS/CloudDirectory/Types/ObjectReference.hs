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
-- Module      : Network.AWS.CloudDirectory.Types.ObjectReference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.ObjectReference where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The reference that identifies an object.
--
-- /See:/ 'newObjectReference' smart constructor.
data ObjectReference = ObjectReference'
  { -- | A path selector supports easy selection of an object by the
    -- parent\/child links leading to it from the directory root. Use the link
    -- names from each parent\/child link to construct the path. Path selectors
    -- start with a slash (\/) and link names are separated by slashes. For
    -- more information about paths, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_access_objects.html Access Objects>.
    -- You can identify an object in one of the following ways:
    --
    -- -   /$ObjectIdentifier/ - An object identifier is an opaque string
    --     provided by Amazon Cloud Directory. When creating objects, the
    --     system will provide you with the identifier of the created object.
    --     An object’s identifier is immutable and no two objects will ever
    --     share the same object identifier
    --
    -- -   /\/some\/path/ - Identifies the object based on path
    --
    -- -   /#SomeBatchReference/ - Identifies the object in a batch call
    selector :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ObjectReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'selector', 'objectReference_selector' - A path selector supports easy selection of an object by the
-- parent\/child links leading to it from the directory root. Use the link
-- names from each parent\/child link to construct the path. Path selectors
-- start with a slash (\/) and link names are separated by slashes. For
-- more information about paths, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_access_objects.html Access Objects>.
-- You can identify an object in one of the following ways:
--
-- -   /$ObjectIdentifier/ - An object identifier is an opaque string
--     provided by Amazon Cloud Directory. When creating objects, the
--     system will provide you with the identifier of the created object.
--     An object’s identifier is immutable and no two objects will ever
--     share the same object identifier
--
-- -   /\/some\/path/ - Identifies the object based on path
--
-- -   /#SomeBatchReference/ - Identifies the object in a batch call
newObjectReference ::
  ObjectReference
newObjectReference =
  ObjectReference' {selector = Prelude.Nothing}

-- | A path selector supports easy selection of an object by the
-- parent\/child links leading to it from the directory root. Use the link
-- names from each parent\/child link to construct the path. Path selectors
-- start with a slash (\/) and link names are separated by slashes. For
-- more information about paths, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_access_objects.html Access Objects>.
-- You can identify an object in one of the following ways:
--
-- -   /$ObjectIdentifier/ - An object identifier is an opaque string
--     provided by Amazon Cloud Directory. When creating objects, the
--     system will provide you with the identifier of the created object.
--     An object’s identifier is immutable and no two objects will ever
--     share the same object identifier
--
-- -   /\/some\/path/ - Identifies the object based on path
--
-- -   /#SomeBatchReference/ - Identifies the object in a batch call
objectReference_selector :: Lens.Lens' ObjectReference (Prelude.Maybe Prelude.Text)
objectReference_selector = Lens.lens (\ObjectReference' {selector} -> selector) (\s@ObjectReference' {} a -> s {selector = a} :: ObjectReference)

instance Prelude.FromJSON ObjectReference where
  parseJSON =
    Prelude.withObject
      "ObjectReference"
      ( \x ->
          ObjectReference'
            Prelude.<$> (x Prelude..:? "Selector")
      )

instance Prelude.Hashable ObjectReference

instance Prelude.NFData ObjectReference

instance Prelude.ToJSON ObjectReference where
  toJSON ObjectReference' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Selector" Prelude..=) Prelude.<$> selector]
      )

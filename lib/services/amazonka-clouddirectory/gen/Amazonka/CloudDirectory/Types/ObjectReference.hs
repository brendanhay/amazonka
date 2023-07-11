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
-- Module      : Amazonka.CloudDirectory.Types.ObjectReference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.ObjectReference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
    --     share the same object identifier. To identify an object with
    --     ObjectIdentifier, the ObjectIdentifier must be wrapped in double
    --     quotes.
    --
    -- -   /\/some\/path/ - Identifies the object based on path
    --
    -- -   /#SomeBatchReference/ - Identifies the object in a batch call
    selector :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
--     share the same object identifier. To identify an object with
--     ObjectIdentifier, the ObjectIdentifier must be wrapped in double
--     quotes.
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
--     share the same object identifier. To identify an object with
--     ObjectIdentifier, the ObjectIdentifier must be wrapped in double
--     quotes.
--
-- -   /\/some\/path/ - Identifies the object based on path
--
-- -   /#SomeBatchReference/ - Identifies the object in a batch call
objectReference_selector :: Lens.Lens' ObjectReference (Prelude.Maybe Prelude.Text)
objectReference_selector = Lens.lens (\ObjectReference' {selector} -> selector) (\s@ObjectReference' {} a -> s {selector = a} :: ObjectReference)

instance Data.FromJSON ObjectReference where
  parseJSON =
    Data.withObject
      "ObjectReference"
      ( \x ->
          ObjectReference' Prelude.<$> (x Data..:? "Selector")
      )

instance Prelude.Hashable ObjectReference where
  hashWithSalt _salt ObjectReference' {..} =
    _salt `Prelude.hashWithSalt` selector

instance Prelude.NFData ObjectReference where
  rnf ObjectReference' {..} = Prelude.rnf selector

instance Data.ToJSON ObjectReference where
  toJSON ObjectReference' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Selector" Data..=) Prelude.<$> selector]
      )

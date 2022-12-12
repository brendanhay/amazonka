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
-- Module      : Amazonka.CloudDirectory.Types.ObjectIdentifierAndLinkNameTuple
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.ObjectIdentifierAndLinkNameTuple where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A pair of ObjectIdentifier and LinkName.
--
-- /See:/ 'newObjectIdentifierAndLinkNameTuple' smart constructor.
data ObjectIdentifierAndLinkNameTuple = ObjectIdentifierAndLinkNameTuple'
  { -- | The name of the link between the parent and the child object.
    linkName :: Prelude.Maybe Prelude.Text,
    -- | The ID that is associated with the object.
    objectIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ObjectIdentifierAndLinkNameTuple' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'linkName', 'objectIdentifierAndLinkNameTuple_linkName' - The name of the link between the parent and the child object.
--
-- 'objectIdentifier', 'objectIdentifierAndLinkNameTuple_objectIdentifier' - The ID that is associated with the object.
newObjectIdentifierAndLinkNameTuple ::
  ObjectIdentifierAndLinkNameTuple
newObjectIdentifierAndLinkNameTuple =
  ObjectIdentifierAndLinkNameTuple'
    { linkName =
        Prelude.Nothing,
      objectIdentifier = Prelude.Nothing
    }

-- | The name of the link between the parent and the child object.
objectIdentifierAndLinkNameTuple_linkName :: Lens.Lens' ObjectIdentifierAndLinkNameTuple (Prelude.Maybe Prelude.Text)
objectIdentifierAndLinkNameTuple_linkName = Lens.lens (\ObjectIdentifierAndLinkNameTuple' {linkName} -> linkName) (\s@ObjectIdentifierAndLinkNameTuple' {} a -> s {linkName = a} :: ObjectIdentifierAndLinkNameTuple)

-- | The ID that is associated with the object.
objectIdentifierAndLinkNameTuple_objectIdentifier :: Lens.Lens' ObjectIdentifierAndLinkNameTuple (Prelude.Maybe Prelude.Text)
objectIdentifierAndLinkNameTuple_objectIdentifier = Lens.lens (\ObjectIdentifierAndLinkNameTuple' {objectIdentifier} -> objectIdentifier) (\s@ObjectIdentifierAndLinkNameTuple' {} a -> s {objectIdentifier = a} :: ObjectIdentifierAndLinkNameTuple)

instance
  Data.FromJSON
    ObjectIdentifierAndLinkNameTuple
  where
  parseJSON =
    Data.withObject
      "ObjectIdentifierAndLinkNameTuple"
      ( \x ->
          ObjectIdentifierAndLinkNameTuple'
            Prelude.<$> (x Data..:? "LinkName")
            Prelude.<*> (x Data..:? "ObjectIdentifier")
      )

instance
  Prelude.Hashable
    ObjectIdentifierAndLinkNameTuple
  where
  hashWithSalt
    _salt
    ObjectIdentifierAndLinkNameTuple' {..} =
      _salt `Prelude.hashWithSalt` linkName
        `Prelude.hashWithSalt` objectIdentifier

instance
  Prelude.NFData
    ObjectIdentifierAndLinkNameTuple
  where
  rnf ObjectIdentifierAndLinkNameTuple' {..} =
    Prelude.rnf linkName
      `Prelude.seq` Prelude.rnf objectIdentifier

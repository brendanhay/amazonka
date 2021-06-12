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
-- Module      : Network.AWS.CloudDirectory.Types.ObjectIdentifierAndLinkNameTuple
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.ObjectIdentifierAndLinkNameTuple where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A pair of ObjectIdentifier and LinkName.
--
-- /See:/ 'newObjectIdentifierAndLinkNameTuple' smart constructor.
data ObjectIdentifierAndLinkNameTuple = ObjectIdentifierAndLinkNameTuple'
  { -- | The name of the link between the parent and the child object.
    linkName :: Core.Maybe Core.Text,
    -- | The ID that is associated with the object.
    objectIdentifier :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      objectIdentifier = Core.Nothing
    }

-- | The name of the link between the parent and the child object.
objectIdentifierAndLinkNameTuple_linkName :: Lens.Lens' ObjectIdentifierAndLinkNameTuple (Core.Maybe Core.Text)
objectIdentifierAndLinkNameTuple_linkName = Lens.lens (\ObjectIdentifierAndLinkNameTuple' {linkName} -> linkName) (\s@ObjectIdentifierAndLinkNameTuple' {} a -> s {linkName = a} :: ObjectIdentifierAndLinkNameTuple)

-- | The ID that is associated with the object.
objectIdentifierAndLinkNameTuple_objectIdentifier :: Lens.Lens' ObjectIdentifierAndLinkNameTuple (Core.Maybe Core.Text)
objectIdentifierAndLinkNameTuple_objectIdentifier = Lens.lens (\ObjectIdentifierAndLinkNameTuple' {objectIdentifier} -> objectIdentifier) (\s@ObjectIdentifierAndLinkNameTuple' {} a -> s {objectIdentifier = a} :: ObjectIdentifierAndLinkNameTuple)

instance
  Core.FromJSON
    ObjectIdentifierAndLinkNameTuple
  where
  parseJSON =
    Core.withObject
      "ObjectIdentifierAndLinkNameTuple"
      ( \x ->
          ObjectIdentifierAndLinkNameTuple'
            Core.<$> (x Core..:? "LinkName")
            Core.<*> (x Core..:? "ObjectIdentifier")
      )

instance
  Core.Hashable
    ObjectIdentifierAndLinkNameTuple

instance Core.NFData ObjectIdentifierAndLinkNameTuple

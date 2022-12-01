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
-- Module      : Amazonka.SSM.Types.InstanceAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InstanceAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | One or more association documents on the managed node.
--
-- /See:/ 'newInstanceAssociation' smart constructor.
data InstanceAssociation = InstanceAssociation'
  { -- | Version information for the association on the managed node.
    associationVersion :: Prelude.Maybe Prelude.Text,
    -- | The managed node ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The content of the association document for the managed node(s).
    content :: Prelude.Maybe Prelude.Text,
    -- | The association ID.
    associationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationVersion', 'instanceAssociation_associationVersion' - Version information for the association on the managed node.
--
-- 'instanceId', 'instanceAssociation_instanceId' - The managed node ID.
--
-- 'content', 'instanceAssociation_content' - The content of the association document for the managed node(s).
--
-- 'associationId', 'instanceAssociation_associationId' - The association ID.
newInstanceAssociation ::
  InstanceAssociation
newInstanceAssociation =
  InstanceAssociation'
    { associationVersion =
        Prelude.Nothing,
      instanceId = Prelude.Nothing,
      content = Prelude.Nothing,
      associationId = Prelude.Nothing
    }

-- | Version information for the association on the managed node.
instanceAssociation_associationVersion :: Lens.Lens' InstanceAssociation (Prelude.Maybe Prelude.Text)
instanceAssociation_associationVersion = Lens.lens (\InstanceAssociation' {associationVersion} -> associationVersion) (\s@InstanceAssociation' {} a -> s {associationVersion = a} :: InstanceAssociation)

-- | The managed node ID.
instanceAssociation_instanceId :: Lens.Lens' InstanceAssociation (Prelude.Maybe Prelude.Text)
instanceAssociation_instanceId = Lens.lens (\InstanceAssociation' {instanceId} -> instanceId) (\s@InstanceAssociation' {} a -> s {instanceId = a} :: InstanceAssociation)

-- | The content of the association document for the managed node(s).
instanceAssociation_content :: Lens.Lens' InstanceAssociation (Prelude.Maybe Prelude.Text)
instanceAssociation_content = Lens.lens (\InstanceAssociation' {content} -> content) (\s@InstanceAssociation' {} a -> s {content = a} :: InstanceAssociation)

-- | The association ID.
instanceAssociation_associationId :: Lens.Lens' InstanceAssociation (Prelude.Maybe Prelude.Text)
instanceAssociation_associationId = Lens.lens (\InstanceAssociation' {associationId} -> associationId) (\s@InstanceAssociation' {} a -> s {associationId = a} :: InstanceAssociation)

instance Core.FromJSON InstanceAssociation where
  parseJSON =
    Core.withObject
      "InstanceAssociation"
      ( \x ->
          InstanceAssociation'
            Prelude.<$> (x Core..:? "AssociationVersion")
            Prelude.<*> (x Core..:? "InstanceId")
            Prelude.<*> (x Core..:? "Content")
            Prelude.<*> (x Core..:? "AssociationId")
      )

instance Prelude.Hashable InstanceAssociation where
  hashWithSalt _salt InstanceAssociation' {..} =
    _salt `Prelude.hashWithSalt` associationVersion
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` associationId

instance Prelude.NFData InstanceAssociation where
  rnf InstanceAssociation' {..} =
    Prelude.rnf associationVersion
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf associationId

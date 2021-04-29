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
-- Module      : Network.AWS.SSM.Types.InstanceAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceAssociation where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | One or more association documents on the instance.
--
-- /See:/ 'newInstanceAssociation' smart constructor.
data InstanceAssociation = InstanceAssociation'
  { -- | The instance ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The association ID.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The content of the association document for the instance(s).
    content :: Prelude.Maybe Prelude.Text,
    -- | Version information for the association on the instance.
    associationVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstanceAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'instanceAssociation_instanceId' - The instance ID.
--
-- 'associationId', 'instanceAssociation_associationId' - The association ID.
--
-- 'content', 'instanceAssociation_content' - The content of the association document for the instance(s).
--
-- 'associationVersion', 'instanceAssociation_associationVersion' - Version information for the association on the instance.
newInstanceAssociation ::
  InstanceAssociation
newInstanceAssociation =
  InstanceAssociation'
    { instanceId = Prelude.Nothing,
      associationId = Prelude.Nothing,
      content = Prelude.Nothing,
      associationVersion = Prelude.Nothing
    }

-- | The instance ID.
instanceAssociation_instanceId :: Lens.Lens' InstanceAssociation (Prelude.Maybe Prelude.Text)
instanceAssociation_instanceId = Lens.lens (\InstanceAssociation' {instanceId} -> instanceId) (\s@InstanceAssociation' {} a -> s {instanceId = a} :: InstanceAssociation)

-- | The association ID.
instanceAssociation_associationId :: Lens.Lens' InstanceAssociation (Prelude.Maybe Prelude.Text)
instanceAssociation_associationId = Lens.lens (\InstanceAssociation' {associationId} -> associationId) (\s@InstanceAssociation' {} a -> s {associationId = a} :: InstanceAssociation)

-- | The content of the association document for the instance(s).
instanceAssociation_content :: Lens.Lens' InstanceAssociation (Prelude.Maybe Prelude.Text)
instanceAssociation_content = Lens.lens (\InstanceAssociation' {content} -> content) (\s@InstanceAssociation' {} a -> s {content = a} :: InstanceAssociation)

-- | Version information for the association on the instance.
instanceAssociation_associationVersion :: Lens.Lens' InstanceAssociation (Prelude.Maybe Prelude.Text)
instanceAssociation_associationVersion = Lens.lens (\InstanceAssociation' {associationVersion} -> associationVersion) (\s@InstanceAssociation' {} a -> s {associationVersion = a} :: InstanceAssociation)

instance Prelude.FromJSON InstanceAssociation where
  parseJSON =
    Prelude.withObject
      "InstanceAssociation"
      ( \x ->
          InstanceAssociation'
            Prelude.<$> (x Prelude..:? "InstanceId")
            Prelude.<*> (x Prelude..:? "AssociationId")
            Prelude.<*> (x Prelude..:? "Content")
            Prelude.<*> (x Prelude..:? "AssociationVersion")
      )

instance Prelude.Hashable InstanceAssociation

instance Prelude.NFData InstanceAssociation

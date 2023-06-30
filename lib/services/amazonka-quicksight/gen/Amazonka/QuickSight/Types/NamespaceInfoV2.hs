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
-- Module      : Amazonka.QuickSight.Types.NamespaceInfoV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NamespaceInfoV2 where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.IdentityStore
import Amazonka.QuickSight.Types.NamespaceError
import Amazonka.QuickSight.Types.NamespaceStatus

-- | The error type.
--
-- /See:/ 'newNamespaceInfoV2' smart constructor.
data NamespaceInfoV2 = NamespaceInfoV2'
  { -- | The namespace ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The namespace Amazon Web Services Region.
    capacityRegion :: Prelude.Maybe Prelude.Text,
    -- | The creation status of a namespace that is not yet completely created.
    creationStatus :: Prelude.Maybe NamespaceStatus,
    -- | The identity store used for the namespace.
    identityStore :: Prelude.Maybe IdentityStore,
    -- | The name of the error.
    name :: Prelude.Maybe Prelude.Text,
    -- | An error that occurred when the namespace was created.
    namespaceError :: Prelude.Maybe NamespaceError
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NamespaceInfoV2' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'namespaceInfoV2_arn' - The namespace ARN.
--
-- 'capacityRegion', 'namespaceInfoV2_capacityRegion' - The namespace Amazon Web Services Region.
--
-- 'creationStatus', 'namespaceInfoV2_creationStatus' - The creation status of a namespace that is not yet completely created.
--
-- 'identityStore', 'namespaceInfoV2_identityStore' - The identity store used for the namespace.
--
-- 'name', 'namespaceInfoV2_name' - The name of the error.
--
-- 'namespaceError', 'namespaceInfoV2_namespaceError' - An error that occurred when the namespace was created.
newNamespaceInfoV2 ::
  NamespaceInfoV2
newNamespaceInfoV2 =
  NamespaceInfoV2'
    { arn = Prelude.Nothing,
      capacityRegion = Prelude.Nothing,
      creationStatus = Prelude.Nothing,
      identityStore = Prelude.Nothing,
      name = Prelude.Nothing,
      namespaceError = Prelude.Nothing
    }

-- | The namespace ARN.
namespaceInfoV2_arn :: Lens.Lens' NamespaceInfoV2 (Prelude.Maybe Prelude.Text)
namespaceInfoV2_arn = Lens.lens (\NamespaceInfoV2' {arn} -> arn) (\s@NamespaceInfoV2' {} a -> s {arn = a} :: NamespaceInfoV2)

-- | The namespace Amazon Web Services Region.
namespaceInfoV2_capacityRegion :: Lens.Lens' NamespaceInfoV2 (Prelude.Maybe Prelude.Text)
namespaceInfoV2_capacityRegion = Lens.lens (\NamespaceInfoV2' {capacityRegion} -> capacityRegion) (\s@NamespaceInfoV2' {} a -> s {capacityRegion = a} :: NamespaceInfoV2)

-- | The creation status of a namespace that is not yet completely created.
namespaceInfoV2_creationStatus :: Lens.Lens' NamespaceInfoV2 (Prelude.Maybe NamespaceStatus)
namespaceInfoV2_creationStatus = Lens.lens (\NamespaceInfoV2' {creationStatus} -> creationStatus) (\s@NamespaceInfoV2' {} a -> s {creationStatus = a} :: NamespaceInfoV2)

-- | The identity store used for the namespace.
namespaceInfoV2_identityStore :: Lens.Lens' NamespaceInfoV2 (Prelude.Maybe IdentityStore)
namespaceInfoV2_identityStore = Lens.lens (\NamespaceInfoV2' {identityStore} -> identityStore) (\s@NamespaceInfoV2' {} a -> s {identityStore = a} :: NamespaceInfoV2)

-- | The name of the error.
namespaceInfoV2_name :: Lens.Lens' NamespaceInfoV2 (Prelude.Maybe Prelude.Text)
namespaceInfoV2_name = Lens.lens (\NamespaceInfoV2' {name} -> name) (\s@NamespaceInfoV2' {} a -> s {name = a} :: NamespaceInfoV2)

-- | An error that occurred when the namespace was created.
namespaceInfoV2_namespaceError :: Lens.Lens' NamespaceInfoV2 (Prelude.Maybe NamespaceError)
namespaceInfoV2_namespaceError = Lens.lens (\NamespaceInfoV2' {namespaceError} -> namespaceError) (\s@NamespaceInfoV2' {} a -> s {namespaceError = a} :: NamespaceInfoV2)

instance Data.FromJSON NamespaceInfoV2 where
  parseJSON =
    Data.withObject
      "NamespaceInfoV2"
      ( \x ->
          NamespaceInfoV2'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CapacityRegion")
            Prelude.<*> (x Data..:? "CreationStatus")
            Prelude.<*> (x Data..:? "IdentityStore")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "NamespaceError")
      )

instance Prelude.Hashable NamespaceInfoV2 where
  hashWithSalt _salt NamespaceInfoV2' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` capacityRegion
      `Prelude.hashWithSalt` creationStatus
      `Prelude.hashWithSalt` identityStore
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` namespaceError

instance Prelude.NFData NamespaceInfoV2 where
  rnf NamespaceInfoV2' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf capacityRegion
      `Prelude.seq` Prelude.rnf creationStatus
      `Prelude.seq` Prelude.rnf identityStore
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf namespaceError

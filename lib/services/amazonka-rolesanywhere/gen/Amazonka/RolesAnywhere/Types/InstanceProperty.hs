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
-- Module      : Amazonka.RolesAnywhere.Types.InstanceProperty
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types.InstanceProperty where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A key-value pair you set that identifies a property of the
-- authenticating instance.
--
-- /See:/ 'newInstanceProperty' smart constructor.
data InstanceProperty = InstanceProperty'
  { -- | Indicates whether the
    -- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
    -- operation was successful.
    failed :: Prelude.Maybe Prelude.Bool,
    -- | The ISO-8601 time stamp of when the certificate was last used in a
    -- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
    -- operation.
    seenAt :: Prelude.Maybe Data.POSIX,
    -- | A list of instanceProperty objects.
    properties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failed', 'instanceProperty_failed' - Indicates whether the
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operation was successful.
--
-- 'seenAt', 'instanceProperty_seenAt' - The ISO-8601 time stamp of when the certificate was last used in a
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operation.
--
-- 'properties', 'instanceProperty_properties' - A list of instanceProperty objects.
newInstanceProperty ::
  InstanceProperty
newInstanceProperty =
  InstanceProperty'
    { failed = Prelude.Nothing,
      seenAt = Prelude.Nothing,
      properties = Prelude.Nothing
    }

-- | Indicates whether the
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operation was successful.
instanceProperty_failed :: Lens.Lens' InstanceProperty (Prelude.Maybe Prelude.Bool)
instanceProperty_failed = Lens.lens (\InstanceProperty' {failed} -> failed) (\s@InstanceProperty' {} a -> s {failed = a} :: InstanceProperty)

-- | The ISO-8601 time stamp of when the certificate was last used in a
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operation.
instanceProperty_seenAt :: Lens.Lens' InstanceProperty (Prelude.Maybe Prelude.UTCTime)
instanceProperty_seenAt = Lens.lens (\InstanceProperty' {seenAt} -> seenAt) (\s@InstanceProperty' {} a -> s {seenAt = a} :: InstanceProperty) Prelude.. Lens.mapping Data._Time

-- | A list of instanceProperty objects.
instanceProperty_properties :: Lens.Lens' InstanceProperty (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
instanceProperty_properties = Lens.lens (\InstanceProperty' {properties} -> properties) (\s@InstanceProperty' {} a -> s {properties = a} :: InstanceProperty) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON InstanceProperty where
  parseJSON =
    Data.withObject
      "InstanceProperty"
      ( \x ->
          InstanceProperty'
            Prelude.<$> (x Data..:? "failed")
            Prelude.<*> (x Data..:? "seenAt")
            Prelude.<*> (x Data..:? "properties" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable InstanceProperty where
  hashWithSalt _salt InstanceProperty' {..} =
    _salt `Prelude.hashWithSalt` failed
      `Prelude.hashWithSalt` seenAt
      `Prelude.hashWithSalt` properties

instance Prelude.NFData InstanceProperty where
  rnf InstanceProperty' {..} =
    Prelude.rnf failed
      `Prelude.seq` Prelude.rnf seenAt
      `Prelude.seq` Prelude.rnf properties

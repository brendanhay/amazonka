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
-- Module      : Amazonka.KeySpaces.Types.KeyspaceSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KeySpaces.Types.KeyspaceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the properties of a keyspace.
--
-- /See:/ 'newKeyspaceSummary' smart constructor.
data KeyspaceSummary = KeyspaceSummary'
  { -- | The name of the keyspace.
    keyspaceName :: Prelude.Text,
    -- | The unique identifier of the keyspace in the format of an Amazon
    -- Resource Name (ARN).
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeyspaceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyspaceName', 'keyspaceSummary_keyspaceName' - The name of the keyspace.
--
-- 'resourceArn', 'keyspaceSummary_resourceArn' - The unique identifier of the keyspace in the format of an Amazon
-- Resource Name (ARN).
newKeyspaceSummary ::
  -- | 'keyspaceName'
  Prelude.Text ->
  -- | 'resourceArn'
  Prelude.Text ->
  KeyspaceSummary
newKeyspaceSummary pKeyspaceName_ pResourceArn_ =
  KeyspaceSummary'
    { keyspaceName = pKeyspaceName_,
      resourceArn = pResourceArn_
    }

-- | The name of the keyspace.
keyspaceSummary_keyspaceName :: Lens.Lens' KeyspaceSummary Prelude.Text
keyspaceSummary_keyspaceName = Lens.lens (\KeyspaceSummary' {keyspaceName} -> keyspaceName) (\s@KeyspaceSummary' {} a -> s {keyspaceName = a} :: KeyspaceSummary)

-- | The unique identifier of the keyspace in the format of an Amazon
-- Resource Name (ARN).
keyspaceSummary_resourceArn :: Lens.Lens' KeyspaceSummary Prelude.Text
keyspaceSummary_resourceArn = Lens.lens (\KeyspaceSummary' {resourceArn} -> resourceArn) (\s@KeyspaceSummary' {} a -> s {resourceArn = a} :: KeyspaceSummary)

instance Data.FromJSON KeyspaceSummary where
  parseJSON =
    Data.withObject
      "KeyspaceSummary"
      ( \x ->
          KeyspaceSummary'
            Prelude.<$> (x Data..: "keyspaceName")
            Prelude.<*> (x Data..: "resourceArn")
      )

instance Prelude.Hashable KeyspaceSummary where
  hashWithSalt _salt KeyspaceSummary' {..} =
    _salt
      `Prelude.hashWithSalt` keyspaceName
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData KeyspaceSummary where
  rnf KeyspaceSummary' {..} =
    Prelude.rnf keyspaceName
      `Prelude.seq` Prelude.rnf resourceArn

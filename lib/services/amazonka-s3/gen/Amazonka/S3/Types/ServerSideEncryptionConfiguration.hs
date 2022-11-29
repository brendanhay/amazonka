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
-- Module      : Amazonka.S3.Types.ServerSideEncryptionConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.ServerSideEncryptionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.ServerSideEncryptionRule

-- | Specifies the default server-side-encryption configuration.
--
-- /See:/ 'newServerSideEncryptionConfiguration' smart constructor.
data ServerSideEncryptionConfiguration = ServerSideEncryptionConfiguration'
  { -- | Container for information about a particular server-side encryption
    -- configuration rule.
    rules :: [ServerSideEncryptionRule]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerSideEncryptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rules', 'serverSideEncryptionConfiguration_rules' - Container for information about a particular server-side encryption
-- configuration rule.
newServerSideEncryptionConfiguration ::
  ServerSideEncryptionConfiguration
newServerSideEncryptionConfiguration =
  ServerSideEncryptionConfiguration'
    { rules =
        Prelude.mempty
    }

-- | Container for information about a particular server-side encryption
-- configuration rule.
serverSideEncryptionConfiguration_rules :: Lens.Lens' ServerSideEncryptionConfiguration [ServerSideEncryptionRule]
serverSideEncryptionConfiguration_rules = Lens.lens (\ServerSideEncryptionConfiguration' {rules} -> rules) (\s@ServerSideEncryptionConfiguration' {} a -> s {rules = a} :: ServerSideEncryptionConfiguration) Prelude.. Lens.coerced

instance
  Core.FromXML
    ServerSideEncryptionConfiguration
  where
  parseXML x =
    ServerSideEncryptionConfiguration'
      Prelude.<$> (Core.parseXMLList "Rule" x)

instance
  Prelude.Hashable
    ServerSideEncryptionConfiguration
  where
  hashWithSalt
    _salt
    ServerSideEncryptionConfiguration' {..} =
      _salt `Prelude.hashWithSalt` rules

instance
  Prelude.NFData
    ServerSideEncryptionConfiguration
  where
  rnf ServerSideEncryptionConfiguration' {..} =
    Prelude.rnf rules

instance Core.ToXML ServerSideEncryptionConfiguration where
  toXML ServerSideEncryptionConfiguration' {..} =
    Prelude.mconcat [Core.toXMLList "Rule" rules]

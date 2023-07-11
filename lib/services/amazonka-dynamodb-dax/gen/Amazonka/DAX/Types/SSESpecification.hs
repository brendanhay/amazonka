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
-- Module      : Amazonka.DAX.Types.SSESpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DAX.Types.SSESpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the settings used to enable server-side encryption.
--
-- /See:/ 'newSSESpecification' smart constructor.
data SSESpecification = SSESpecification'
  { -- | Indicates whether server-side encryption is enabled (true) or disabled
    -- (false) on the cluster.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SSESpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'sSESpecification_enabled' - Indicates whether server-side encryption is enabled (true) or disabled
-- (false) on the cluster.
newSSESpecification ::
  -- | 'enabled'
  Prelude.Bool ->
  SSESpecification
newSSESpecification pEnabled_ =
  SSESpecification' {enabled = pEnabled_}

-- | Indicates whether server-side encryption is enabled (true) or disabled
-- (false) on the cluster.
sSESpecification_enabled :: Lens.Lens' SSESpecification Prelude.Bool
sSESpecification_enabled = Lens.lens (\SSESpecification' {enabled} -> enabled) (\s@SSESpecification' {} a -> s {enabled = a} :: SSESpecification)

instance Prelude.Hashable SSESpecification where
  hashWithSalt _salt SSESpecification' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData SSESpecification where
  rnf SSESpecification' {..} = Prelude.rnf enabled

instance Data.ToJSON SSESpecification where
  toJSON SSESpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Enabled" Data..= enabled)]
      )

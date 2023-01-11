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
-- Module      : Amazonka.Kafka.Types.PublicAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.PublicAccess where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Public access control for brokers.
--
-- /See:/ 'newPublicAccess' smart constructor.
data PublicAccess = PublicAccess'
  { -- | The value DISABLED indicates that public access is turned off.
    -- SERVICE_PROVIDED_EIPS indicates that public access is turned on.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublicAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'publicAccess_type' - The value DISABLED indicates that public access is turned off.
-- SERVICE_PROVIDED_EIPS indicates that public access is turned on.
newPublicAccess ::
  PublicAccess
newPublicAccess =
  PublicAccess' {type' = Prelude.Nothing}

-- | The value DISABLED indicates that public access is turned off.
-- SERVICE_PROVIDED_EIPS indicates that public access is turned on.
publicAccess_type :: Lens.Lens' PublicAccess (Prelude.Maybe Prelude.Text)
publicAccess_type = Lens.lens (\PublicAccess' {type'} -> type') (\s@PublicAccess' {} a -> s {type' = a} :: PublicAccess)

instance Data.FromJSON PublicAccess where
  parseJSON =
    Data.withObject
      "PublicAccess"
      ( \x ->
          PublicAccess' Prelude.<$> (x Data..:? "type")
      )

instance Prelude.Hashable PublicAccess where
  hashWithSalt _salt PublicAccess' {..} =
    _salt `Prelude.hashWithSalt` type'

instance Prelude.NFData PublicAccess where
  rnf PublicAccess' {..} = Prelude.rnf type'

instance Data.ToJSON PublicAccess where
  toJSON PublicAccess' {..} =
    Data.object
      ( Prelude.catMaybes
          [("type" Data..=) Prelude.<$> type']
      )

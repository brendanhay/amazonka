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
-- Module      : Amazonka.Kafka.Types.Iam
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.Iam where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details for IAM access control.
--
-- /See:/ 'newIam' smart constructor.
data Iam = Iam'
  { -- | Indicates whether IAM access control is enabled.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Iam' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'iam_enabled' - Indicates whether IAM access control is enabled.
newIam ::
  Iam
newIam = Iam' {enabled = Prelude.Nothing}

-- | Indicates whether IAM access control is enabled.
iam_enabled :: Lens.Lens' Iam (Prelude.Maybe Prelude.Bool)
iam_enabled = Lens.lens (\Iam' {enabled} -> enabled) (\s@Iam' {} a -> s {enabled = a} :: Iam)

instance Data.FromJSON Iam where
  parseJSON =
    Data.withObject
      "Iam"
      (\x -> Iam' Prelude.<$> (x Data..:? "enabled"))

instance Prelude.Hashable Iam where
  hashWithSalt _salt Iam' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData Iam where
  rnf Iam' {..} = Prelude.rnf enabled

instance Data.ToJSON Iam where
  toJSON Iam' {..} =
    Data.object
      ( Prelude.catMaybes
          [("enabled" Data..=) Prelude.<$> enabled]
      )

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
-- Module      : Amazonka.Kafka.Types.ServerlessSasl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.ServerlessSasl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.Iam
import qualified Amazonka.Prelude as Prelude

-- | Details for client authentication using SASL.
--
-- /See:/ 'newServerlessSasl' smart constructor.
data ServerlessSasl = ServerlessSasl'
  { -- | Indicates whether IAM access control is enabled.
    iam :: Prelude.Maybe Iam
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerlessSasl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iam', 'serverlessSasl_iam' - Indicates whether IAM access control is enabled.
newServerlessSasl ::
  ServerlessSasl
newServerlessSasl =
  ServerlessSasl' {iam = Prelude.Nothing}

-- | Indicates whether IAM access control is enabled.
serverlessSasl_iam :: Lens.Lens' ServerlessSasl (Prelude.Maybe Iam)
serverlessSasl_iam = Lens.lens (\ServerlessSasl' {iam} -> iam) (\s@ServerlessSasl' {} a -> s {iam = a} :: ServerlessSasl)

instance Data.FromJSON ServerlessSasl where
  parseJSON =
    Data.withObject
      "ServerlessSasl"
      ( \x ->
          ServerlessSasl' Prelude.<$> (x Data..:? "iam")
      )

instance Prelude.Hashable ServerlessSasl where
  hashWithSalt _salt ServerlessSasl' {..} =
    _salt `Prelude.hashWithSalt` iam

instance Prelude.NFData ServerlessSasl where
  rnf ServerlessSasl' {..} = Prelude.rnf iam

instance Data.ToJSON ServerlessSasl where
  toJSON ServerlessSasl' {..} =
    Data.object
      (Prelude.catMaybes [("iam" Data..=) Prelude.<$> iam])

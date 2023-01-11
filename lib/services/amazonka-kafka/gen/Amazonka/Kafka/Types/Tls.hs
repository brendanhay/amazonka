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
-- Module      : Amazonka.Kafka.Types.Tls
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.Tls where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details for client authentication using TLS.
--
-- /See:/ 'newTls' smart constructor.
data Tls = Tls'
  { -- | List of ACM Certificate Authority ARNs.
    certificateAuthorityArnList :: Prelude.Maybe [Prelude.Text],
    -- | Specifies whether you want to turn on or turn off TLS authentication.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Tls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityArnList', 'tls_certificateAuthorityArnList' - List of ACM Certificate Authority ARNs.
--
-- 'enabled', 'tls_enabled' - Specifies whether you want to turn on or turn off TLS authentication.
newTls ::
  Tls
newTls =
  Tls'
    { certificateAuthorityArnList = Prelude.Nothing,
      enabled = Prelude.Nothing
    }

-- | List of ACM Certificate Authority ARNs.
tls_certificateAuthorityArnList :: Lens.Lens' Tls (Prelude.Maybe [Prelude.Text])
tls_certificateAuthorityArnList = Lens.lens (\Tls' {certificateAuthorityArnList} -> certificateAuthorityArnList) (\s@Tls' {} a -> s {certificateAuthorityArnList = a} :: Tls) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether you want to turn on or turn off TLS authentication.
tls_enabled :: Lens.Lens' Tls (Prelude.Maybe Prelude.Bool)
tls_enabled = Lens.lens (\Tls' {enabled} -> enabled) (\s@Tls' {} a -> s {enabled = a} :: Tls)

instance Data.FromJSON Tls where
  parseJSON =
    Data.withObject
      "Tls"
      ( \x ->
          Tls'
            Prelude.<$> ( x Data..:? "certificateAuthorityArnList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "enabled")
      )

instance Prelude.Hashable Tls where
  hashWithSalt _salt Tls' {..} =
    _salt
      `Prelude.hashWithSalt` certificateAuthorityArnList
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData Tls where
  rnf Tls' {..} =
    Prelude.rnf certificateAuthorityArnList
      `Prelude.seq` Prelude.rnf enabled

instance Data.ToJSON Tls where
  toJSON Tls' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("certificateAuthorityArnList" Data..=)
              Prelude.<$> certificateAuthorityArnList,
            ("enabled" Data..=) Prelude.<$> enabled
          ]
      )

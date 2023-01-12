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
-- Module      : Amazonka.Batch.Types.EksSecret
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.EksSecret where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration of a Kubernetes @secret@ volume. For more
-- information, see
-- <https://kubernetes.io/docs/concepts/storage/volumes/#secret secret> in
-- the /Kubernetes documentation/.
--
-- /See:/ 'newEksSecret' smart constructor.
data EksSecret = EksSecret'
  { -- | Specifies whether the secret or the secret\'s keys must be defined.
    optional :: Prelude.Maybe Prelude.Bool,
    -- | The name of the secret. The name must be allowed as a DNS subdomain
    -- name. For more information, see
    -- <https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#dns-subdomain-names DNS subdomain names>
    -- in the /Kubernetes documentation/.
    secretName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksSecret' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optional', 'eksSecret_optional' - Specifies whether the secret or the secret\'s keys must be defined.
--
-- 'secretName', 'eksSecret_secretName' - The name of the secret. The name must be allowed as a DNS subdomain
-- name. For more information, see
-- <https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#dns-subdomain-names DNS subdomain names>
-- in the /Kubernetes documentation/.
newEksSecret ::
  -- | 'secretName'
  Prelude.Text ->
  EksSecret
newEksSecret pSecretName_ =
  EksSecret'
    { optional = Prelude.Nothing,
      secretName = pSecretName_
    }

-- | Specifies whether the secret or the secret\'s keys must be defined.
eksSecret_optional :: Lens.Lens' EksSecret (Prelude.Maybe Prelude.Bool)
eksSecret_optional = Lens.lens (\EksSecret' {optional} -> optional) (\s@EksSecret' {} a -> s {optional = a} :: EksSecret)

-- | The name of the secret. The name must be allowed as a DNS subdomain
-- name. For more information, see
-- <https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#dns-subdomain-names DNS subdomain names>
-- in the /Kubernetes documentation/.
eksSecret_secretName :: Lens.Lens' EksSecret Prelude.Text
eksSecret_secretName = Lens.lens (\EksSecret' {secretName} -> secretName) (\s@EksSecret' {} a -> s {secretName = a} :: EksSecret)

instance Data.FromJSON EksSecret where
  parseJSON =
    Data.withObject
      "EksSecret"
      ( \x ->
          EksSecret'
            Prelude.<$> (x Data..:? "optional")
            Prelude.<*> (x Data..: "secretName")
      )

instance Prelude.Hashable EksSecret where
  hashWithSalt _salt EksSecret' {..} =
    _salt `Prelude.hashWithSalt` optional
      `Prelude.hashWithSalt` secretName

instance Prelude.NFData EksSecret where
  rnf EksSecret' {..} =
    Prelude.rnf optional
      `Prelude.seq` Prelude.rnf secretName

instance Data.ToJSON EksSecret where
  toJSON EksSecret' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("optional" Data..=) Prelude.<$> optional,
            Prelude.Just ("secretName" Data..= secretName)
          ]
      )

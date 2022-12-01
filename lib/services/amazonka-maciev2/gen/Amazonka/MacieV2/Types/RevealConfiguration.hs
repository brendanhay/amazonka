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
-- Module      : Amazonka.MacieV2.Types.RevealConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.RevealConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types.RevealStatus
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration settings for retrieving occurrences of
-- sensitive data reported by findings, and the status of the configuration
-- for an Amazon Macie account. When you enable the configuration for the
-- first time, your request must specify an Key Management Service (KMS)
-- key. Otherwise, an error occurs. Macie uses the specified key to encrypt
-- the sensitive data that you retrieve.
--
-- /See:/ 'newRevealConfiguration' smart constructor.
data RevealConfiguration = RevealConfiguration'
  { -- | The Amazon Resource Name (ARN), ID, or alias of the KMS key to use to
    -- encrypt sensitive data that\'s retrieved. The key must be an existing,
    -- customer managed, symmetric encryption key that\'s in the same Amazon
    -- Web Services Region as the Amazon Macie account.
    --
    -- If this value specifies an alias, it must include the following prefix:
    -- alias\/. If this value specifies a key that\'s owned by another Amazon
    -- Web Services account, it must specify the ARN of the key or the ARN of
    -- the key\'s alias.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The status of the configuration for the Amazon Macie account. In a
    -- request, valid values are: ENABLED, enable the configuration for the
    -- account; and, DISABLED, disable the configuration for the account. In a
    -- response, possible values are: ENABLED, the configuration is currently
    -- enabled for the account; and, DISABLED, the configuration is currently
    -- disabled for the account.
    status :: RevealStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevealConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'revealConfiguration_kmsKeyId' - The Amazon Resource Name (ARN), ID, or alias of the KMS key to use to
-- encrypt sensitive data that\'s retrieved. The key must be an existing,
-- customer managed, symmetric encryption key that\'s in the same Amazon
-- Web Services Region as the Amazon Macie account.
--
-- If this value specifies an alias, it must include the following prefix:
-- alias\/. If this value specifies a key that\'s owned by another Amazon
-- Web Services account, it must specify the ARN of the key or the ARN of
-- the key\'s alias.
--
-- 'status', 'revealConfiguration_status' - The status of the configuration for the Amazon Macie account. In a
-- request, valid values are: ENABLED, enable the configuration for the
-- account; and, DISABLED, disable the configuration for the account. In a
-- response, possible values are: ENABLED, the configuration is currently
-- enabled for the account; and, DISABLED, the configuration is currently
-- disabled for the account.
newRevealConfiguration ::
  -- | 'status'
  RevealStatus ->
  RevealConfiguration
newRevealConfiguration pStatus_ =
  RevealConfiguration'
    { kmsKeyId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN), ID, or alias of the KMS key to use to
-- encrypt sensitive data that\'s retrieved. The key must be an existing,
-- customer managed, symmetric encryption key that\'s in the same Amazon
-- Web Services Region as the Amazon Macie account.
--
-- If this value specifies an alias, it must include the following prefix:
-- alias\/. If this value specifies a key that\'s owned by another Amazon
-- Web Services account, it must specify the ARN of the key or the ARN of
-- the key\'s alias.
revealConfiguration_kmsKeyId :: Lens.Lens' RevealConfiguration (Prelude.Maybe Prelude.Text)
revealConfiguration_kmsKeyId = Lens.lens (\RevealConfiguration' {kmsKeyId} -> kmsKeyId) (\s@RevealConfiguration' {} a -> s {kmsKeyId = a} :: RevealConfiguration)

-- | The status of the configuration for the Amazon Macie account. In a
-- request, valid values are: ENABLED, enable the configuration for the
-- account; and, DISABLED, disable the configuration for the account. In a
-- response, possible values are: ENABLED, the configuration is currently
-- enabled for the account; and, DISABLED, the configuration is currently
-- disabled for the account.
revealConfiguration_status :: Lens.Lens' RevealConfiguration RevealStatus
revealConfiguration_status = Lens.lens (\RevealConfiguration' {status} -> status) (\s@RevealConfiguration' {} a -> s {status = a} :: RevealConfiguration)

instance Core.FromJSON RevealConfiguration where
  parseJSON =
    Core.withObject
      "RevealConfiguration"
      ( \x ->
          RevealConfiguration'
            Prelude.<$> (x Core..:? "kmsKeyId")
            Prelude.<*> (x Core..: "status")
      )

instance Prelude.Hashable RevealConfiguration where
  hashWithSalt _salt RevealConfiguration' {..} =
    _salt `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` status

instance Prelude.NFData RevealConfiguration where
  rnf RevealConfiguration' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf status

instance Core.ToJSON RevealConfiguration where
  toJSON RevealConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("kmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            Prelude.Just ("status" Core..= status)
          ]
      )

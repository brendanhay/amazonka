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
-- Module      : Amazonka.SecurityHub.Types.AwsElasticsearchDomainNodeToNodeEncryptionOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElasticsearchDomainNodeToNodeEncryptionOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about the configuration for node-to-node encryption.
--
-- /See:/ 'newAwsElasticsearchDomainNodeToNodeEncryptionOptions' smart constructor.
data AwsElasticsearchDomainNodeToNodeEncryptionOptions = AwsElasticsearchDomainNodeToNodeEncryptionOptions'
  { -- | Whether node-to-node encryption is enabled.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElasticsearchDomainNodeToNodeEncryptionOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'awsElasticsearchDomainNodeToNodeEncryptionOptions_enabled' - Whether node-to-node encryption is enabled.
newAwsElasticsearchDomainNodeToNodeEncryptionOptions ::
  AwsElasticsearchDomainNodeToNodeEncryptionOptions
newAwsElasticsearchDomainNodeToNodeEncryptionOptions =
  AwsElasticsearchDomainNodeToNodeEncryptionOptions'
    { enabled =
        Prelude.Nothing
    }

-- | Whether node-to-node encryption is enabled.
awsElasticsearchDomainNodeToNodeEncryptionOptions_enabled :: Lens.Lens' AwsElasticsearchDomainNodeToNodeEncryptionOptions (Prelude.Maybe Prelude.Bool)
awsElasticsearchDomainNodeToNodeEncryptionOptions_enabled = Lens.lens (\AwsElasticsearchDomainNodeToNodeEncryptionOptions' {enabled} -> enabled) (\s@AwsElasticsearchDomainNodeToNodeEncryptionOptions' {} a -> s {enabled = a} :: AwsElasticsearchDomainNodeToNodeEncryptionOptions)

instance
  Core.FromJSON
    AwsElasticsearchDomainNodeToNodeEncryptionOptions
  where
  parseJSON =
    Core.withObject
      "AwsElasticsearchDomainNodeToNodeEncryptionOptions"
      ( \x ->
          AwsElasticsearchDomainNodeToNodeEncryptionOptions'
            Prelude.<$> (x Core..:? "Enabled")
      )

instance
  Prelude.Hashable
    AwsElasticsearchDomainNodeToNodeEncryptionOptions
  where
  hashWithSalt
    _salt
    AwsElasticsearchDomainNodeToNodeEncryptionOptions' {..} =
      _salt `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    AwsElasticsearchDomainNodeToNodeEncryptionOptions
  where
  rnf
    AwsElasticsearchDomainNodeToNodeEncryptionOptions' {..} =
      Prelude.rnf enabled

instance
  Core.ToJSON
    AwsElasticsearchDomainNodeToNodeEncryptionOptions
  where
  toJSON
    AwsElasticsearchDomainNodeToNodeEncryptionOptions' {..} =
      Core.object
        ( Prelude.catMaybes
            [("Enabled" Core..=) Prelude.<$> enabled]
        )

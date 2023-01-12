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
-- Module      : Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides details about the configuration for node-to-node encryption.
--
-- /See:/ 'newAwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails' smart constructor.
data AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails = AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails'
  { -- | Whether node-to-node encryption is enabled.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'awsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails_enabled' - Whether node-to-node encryption is enabled.
newAwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails ::
  AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails
newAwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails =
  AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails'
    { enabled =
        Prelude.Nothing
    }

-- | Whether node-to-node encryption is enabled.
awsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails_enabled :: Lens.Lens' AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails (Prelude.Maybe Prelude.Bool)
awsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails_enabled = Lens.lens (\AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails' {enabled} -> enabled) (\s@AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails' {} a -> s {enabled = a} :: AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails)

instance
  Data.FromJSON
    AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails
  where
  parseJSON =
    Data.withObject
      "AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails"
      ( \x ->
          AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails'
            Prelude.<$> (x Data..:? "Enabled")
      )

instance
  Prelude.Hashable
    AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails
  where
  hashWithSalt
    _salt
    AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails' {..} =
      _salt `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails
  where
  rnf
    AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails' {..} =
      Prelude.rnf enabled

instance
  Data.ToJSON
    AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails
  where
  toJSON
    AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [("Enabled" Data..=) Prelude.<$> enabled]
        )

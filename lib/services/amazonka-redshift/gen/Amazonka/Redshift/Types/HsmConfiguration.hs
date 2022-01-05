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
-- Module      : Amazonka.Redshift.Types.HsmConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.HsmConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.Tag

-- | Returns information about an HSM configuration, which is an object that
-- describes to Amazon Redshift clusters the information they require to
-- connect to an HSM where they can store database encryption keys.
--
-- /See:/ 'newHsmConfiguration' smart constructor.
data HsmConfiguration = HsmConfiguration'
  { -- | The name of the Amazon Redshift HSM configuration.
    hsmConfigurationIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the partition in the HSM where the Amazon Redshift clusters
    -- will store their database encryption keys.
    hsmPartitionName :: Prelude.Maybe Prelude.Text,
    -- | A text description of the HSM configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The list of tags for the HSM configuration.
    tags :: Prelude.Maybe [Tag],
    -- | The IP address that the Amazon Redshift cluster must use to access the
    -- HSM.
    hsmIpAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HsmConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hsmConfigurationIdentifier', 'hsmConfiguration_hsmConfigurationIdentifier' - The name of the Amazon Redshift HSM configuration.
--
-- 'hsmPartitionName', 'hsmConfiguration_hsmPartitionName' - The name of the partition in the HSM where the Amazon Redshift clusters
-- will store their database encryption keys.
--
-- 'description', 'hsmConfiguration_description' - A text description of the HSM configuration.
--
-- 'tags', 'hsmConfiguration_tags' - The list of tags for the HSM configuration.
--
-- 'hsmIpAddress', 'hsmConfiguration_hsmIpAddress' - The IP address that the Amazon Redshift cluster must use to access the
-- HSM.
newHsmConfiguration ::
  HsmConfiguration
newHsmConfiguration =
  HsmConfiguration'
    { hsmConfigurationIdentifier =
        Prelude.Nothing,
      hsmPartitionName = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      hsmIpAddress = Prelude.Nothing
    }

-- | The name of the Amazon Redshift HSM configuration.
hsmConfiguration_hsmConfigurationIdentifier :: Lens.Lens' HsmConfiguration (Prelude.Maybe Prelude.Text)
hsmConfiguration_hsmConfigurationIdentifier = Lens.lens (\HsmConfiguration' {hsmConfigurationIdentifier} -> hsmConfigurationIdentifier) (\s@HsmConfiguration' {} a -> s {hsmConfigurationIdentifier = a} :: HsmConfiguration)

-- | The name of the partition in the HSM where the Amazon Redshift clusters
-- will store their database encryption keys.
hsmConfiguration_hsmPartitionName :: Lens.Lens' HsmConfiguration (Prelude.Maybe Prelude.Text)
hsmConfiguration_hsmPartitionName = Lens.lens (\HsmConfiguration' {hsmPartitionName} -> hsmPartitionName) (\s@HsmConfiguration' {} a -> s {hsmPartitionName = a} :: HsmConfiguration)

-- | A text description of the HSM configuration.
hsmConfiguration_description :: Lens.Lens' HsmConfiguration (Prelude.Maybe Prelude.Text)
hsmConfiguration_description = Lens.lens (\HsmConfiguration' {description} -> description) (\s@HsmConfiguration' {} a -> s {description = a} :: HsmConfiguration)

-- | The list of tags for the HSM configuration.
hsmConfiguration_tags :: Lens.Lens' HsmConfiguration (Prelude.Maybe [Tag])
hsmConfiguration_tags = Lens.lens (\HsmConfiguration' {tags} -> tags) (\s@HsmConfiguration' {} a -> s {tags = a} :: HsmConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The IP address that the Amazon Redshift cluster must use to access the
-- HSM.
hsmConfiguration_hsmIpAddress :: Lens.Lens' HsmConfiguration (Prelude.Maybe Prelude.Text)
hsmConfiguration_hsmIpAddress = Lens.lens (\HsmConfiguration' {hsmIpAddress} -> hsmIpAddress) (\s@HsmConfiguration' {} a -> s {hsmIpAddress = a} :: HsmConfiguration)

instance Core.FromXML HsmConfiguration where
  parseXML x =
    HsmConfiguration'
      Prelude.<$> (x Core..@? "HsmConfigurationIdentifier")
      Prelude.<*> (x Core..@? "HsmPartitionName")
      Prelude.<*> (x Core..@? "Description")
      Prelude.<*> ( x Core..@? "Tags" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "Tag")
                  )
      Prelude.<*> (x Core..@? "HsmIpAddress")

instance Prelude.Hashable HsmConfiguration where
  hashWithSalt _salt HsmConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` hsmConfigurationIdentifier
      `Prelude.hashWithSalt` hsmPartitionName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` hsmIpAddress

instance Prelude.NFData HsmConfiguration where
  rnf HsmConfiguration' {..} =
    Prelude.rnf hsmConfigurationIdentifier
      `Prelude.seq` Prelude.rnf hsmPartitionName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf hsmIpAddress

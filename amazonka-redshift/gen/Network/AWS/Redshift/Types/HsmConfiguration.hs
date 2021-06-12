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
-- Module      : Network.AWS.Redshift.Types.HsmConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.HsmConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag

-- | Returns information about an HSM configuration, which is an object that
-- describes to Amazon Redshift clusters the information they require to
-- connect to an HSM where they can store database encryption keys.
--
-- /See:/ 'newHsmConfiguration' smart constructor.
data HsmConfiguration = HsmConfiguration'
  { -- | The list of tags for the HSM configuration.
    tags :: Core.Maybe [Tag],
    -- | The IP address that the Amazon Redshift cluster must use to access the
    -- HSM.
    hsmIpAddress :: Core.Maybe Core.Text,
    -- | A text description of the HSM configuration.
    description :: Core.Maybe Core.Text,
    -- | The name of the partition in the HSM where the Amazon Redshift clusters
    -- will store their database encryption keys.
    hsmPartitionName :: Core.Maybe Core.Text,
    -- | The name of the Amazon Redshift HSM configuration.
    hsmConfigurationIdentifier :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HsmConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'hsmConfiguration_tags' - The list of tags for the HSM configuration.
--
-- 'hsmIpAddress', 'hsmConfiguration_hsmIpAddress' - The IP address that the Amazon Redshift cluster must use to access the
-- HSM.
--
-- 'description', 'hsmConfiguration_description' - A text description of the HSM configuration.
--
-- 'hsmPartitionName', 'hsmConfiguration_hsmPartitionName' - The name of the partition in the HSM where the Amazon Redshift clusters
-- will store their database encryption keys.
--
-- 'hsmConfigurationIdentifier', 'hsmConfiguration_hsmConfigurationIdentifier' - The name of the Amazon Redshift HSM configuration.
newHsmConfiguration ::
  HsmConfiguration
newHsmConfiguration =
  HsmConfiguration'
    { tags = Core.Nothing,
      hsmIpAddress = Core.Nothing,
      description = Core.Nothing,
      hsmPartitionName = Core.Nothing,
      hsmConfigurationIdentifier = Core.Nothing
    }

-- | The list of tags for the HSM configuration.
hsmConfiguration_tags :: Lens.Lens' HsmConfiguration (Core.Maybe [Tag])
hsmConfiguration_tags = Lens.lens (\HsmConfiguration' {tags} -> tags) (\s@HsmConfiguration' {} a -> s {tags = a} :: HsmConfiguration) Core.. Lens.mapping Lens._Coerce

-- | The IP address that the Amazon Redshift cluster must use to access the
-- HSM.
hsmConfiguration_hsmIpAddress :: Lens.Lens' HsmConfiguration (Core.Maybe Core.Text)
hsmConfiguration_hsmIpAddress = Lens.lens (\HsmConfiguration' {hsmIpAddress} -> hsmIpAddress) (\s@HsmConfiguration' {} a -> s {hsmIpAddress = a} :: HsmConfiguration)

-- | A text description of the HSM configuration.
hsmConfiguration_description :: Lens.Lens' HsmConfiguration (Core.Maybe Core.Text)
hsmConfiguration_description = Lens.lens (\HsmConfiguration' {description} -> description) (\s@HsmConfiguration' {} a -> s {description = a} :: HsmConfiguration)

-- | The name of the partition in the HSM where the Amazon Redshift clusters
-- will store their database encryption keys.
hsmConfiguration_hsmPartitionName :: Lens.Lens' HsmConfiguration (Core.Maybe Core.Text)
hsmConfiguration_hsmPartitionName = Lens.lens (\HsmConfiguration' {hsmPartitionName} -> hsmPartitionName) (\s@HsmConfiguration' {} a -> s {hsmPartitionName = a} :: HsmConfiguration)

-- | The name of the Amazon Redshift HSM configuration.
hsmConfiguration_hsmConfigurationIdentifier :: Lens.Lens' HsmConfiguration (Core.Maybe Core.Text)
hsmConfiguration_hsmConfigurationIdentifier = Lens.lens (\HsmConfiguration' {hsmConfigurationIdentifier} -> hsmConfigurationIdentifier) (\s@HsmConfiguration' {} a -> s {hsmConfigurationIdentifier = a} :: HsmConfiguration)

instance Core.FromXML HsmConfiguration where
  parseXML x =
    HsmConfiguration'
      Core.<$> ( x Core..@? "Tags" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Tag")
               )
      Core.<*> (x Core..@? "HsmIpAddress")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "HsmPartitionName")
      Core.<*> (x Core..@? "HsmConfigurationIdentifier")

instance Core.Hashable HsmConfiguration

instance Core.NFData HsmConfiguration

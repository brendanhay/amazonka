{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag

-- | Returns information about an HSM configuration, which is an object that
-- describes to Amazon Redshift clusters the information they require to
-- connect to an HSM where they can store database encryption keys.
--
-- /See:/ 'newHsmConfiguration' smart constructor.
data HsmConfiguration = HsmConfiguration'
  { -- | The list of tags for the HSM configuration.
    tags :: Prelude.Maybe [Tag],
    -- | The IP address that the Amazon Redshift cluster must use to access the
    -- HSM.
    hsmIpAddress :: Prelude.Maybe Prelude.Text,
    -- | A text description of the HSM configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the partition in the HSM where the Amazon Redshift clusters
    -- will store their database encryption keys.
    hsmPartitionName :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon Redshift HSM configuration.
    hsmConfigurationIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { tags = Prelude.Nothing,
      hsmIpAddress = Prelude.Nothing,
      description = Prelude.Nothing,
      hsmPartitionName = Prelude.Nothing,
      hsmConfigurationIdentifier = Prelude.Nothing
    }

-- | The list of tags for the HSM configuration.
hsmConfiguration_tags :: Lens.Lens' HsmConfiguration (Prelude.Maybe [Tag])
hsmConfiguration_tags = Lens.lens (\HsmConfiguration' {tags} -> tags) (\s@HsmConfiguration' {} a -> s {tags = a} :: HsmConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | The IP address that the Amazon Redshift cluster must use to access the
-- HSM.
hsmConfiguration_hsmIpAddress :: Lens.Lens' HsmConfiguration (Prelude.Maybe Prelude.Text)
hsmConfiguration_hsmIpAddress = Lens.lens (\HsmConfiguration' {hsmIpAddress} -> hsmIpAddress) (\s@HsmConfiguration' {} a -> s {hsmIpAddress = a} :: HsmConfiguration)

-- | A text description of the HSM configuration.
hsmConfiguration_description :: Lens.Lens' HsmConfiguration (Prelude.Maybe Prelude.Text)
hsmConfiguration_description = Lens.lens (\HsmConfiguration' {description} -> description) (\s@HsmConfiguration' {} a -> s {description = a} :: HsmConfiguration)

-- | The name of the partition in the HSM where the Amazon Redshift clusters
-- will store their database encryption keys.
hsmConfiguration_hsmPartitionName :: Lens.Lens' HsmConfiguration (Prelude.Maybe Prelude.Text)
hsmConfiguration_hsmPartitionName = Lens.lens (\HsmConfiguration' {hsmPartitionName} -> hsmPartitionName) (\s@HsmConfiguration' {} a -> s {hsmPartitionName = a} :: HsmConfiguration)

-- | The name of the Amazon Redshift HSM configuration.
hsmConfiguration_hsmConfigurationIdentifier :: Lens.Lens' HsmConfiguration (Prelude.Maybe Prelude.Text)
hsmConfiguration_hsmConfigurationIdentifier = Lens.lens (\HsmConfiguration' {hsmConfigurationIdentifier} -> hsmConfigurationIdentifier) (\s@HsmConfiguration' {} a -> s {hsmConfigurationIdentifier = a} :: HsmConfiguration)

instance Prelude.FromXML HsmConfiguration where
  parseXML x =
    HsmConfiguration'
      Prelude.<$> ( x Prelude..@? "Tags" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "Tag")
                  )
      Prelude.<*> (x Prelude..@? "HsmIpAddress")
      Prelude.<*> (x Prelude..@? "Description")
      Prelude.<*> (x Prelude..@? "HsmPartitionName")
      Prelude.<*> (x Prelude..@? "HsmConfigurationIdentifier")

instance Prelude.Hashable HsmConfiguration

instance Prelude.NFData HsmConfiguration

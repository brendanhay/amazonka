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
-- Module      : Amazonka.Inspector2.Types.Ec2Metadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.Ec2Metadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.Ec2Platform
import qualified Amazonka.Prelude as Prelude

-- | Meta data details of an Amazon EC2 instance.
--
-- /See:/ 'newEc2Metadata' smart constructor.
data Ec2Metadata = Ec2Metadata'
  { -- | The ID of the Amazon Machine Image (AMI) used to launch the instance.
    amiId :: Prelude.Maybe Prelude.Text,
    -- | The platform of the instance.
    platform :: Prelude.Maybe Ec2Platform,
    -- | The tags attached to the instance.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ec2Metadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amiId', 'ec2Metadata_amiId' - The ID of the Amazon Machine Image (AMI) used to launch the instance.
--
-- 'platform', 'ec2Metadata_platform' - The platform of the instance.
--
-- 'tags', 'ec2Metadata_tags' - The tags attached to the instance.
newEc2Metadata ::
  Ec2Metadata
newEc2Metadata =
  Ec2Metadata'
    { amiId = Prelude.Nothing,
      platform = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ID of the Amazon Machine Image (AMI) used to launch the instance.
ec2Metadata_amiId :: Lens.Lens' Ec2Metadata (Prelude.Maybe Prelude.Text)
ec2Metadata_amiId = Lens.lens (\Ec2Metadata' {amiId} -> amiId) (\s@Ec2Metadata' {} a -> s {amiId = a} :: Ec2Metadata)

-- | The platform of the instance.
ec2Metadata_platform :: Lens.Lens' Ec2Metadata (Prelude.Maybe Ec2Platform)
ec2Metadata_platform = Lens.lens (\Ec2Metadata' {platform} -> platform) (\s@Ec2Metadata' {} a -> s {platform = a} :: Ec2Metadata)

-- | The tags attached to the instance.
ec2Metadata_tags :: Lens.Lens' Ec2Metadata (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
ec2Metadata_tags = Lens.lens (\Ec2Metadata' {tags} -> tags) (\s@Ec2Metadata' {} a -> s {tags = a} :: Ec2Metadata) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Ec2Metadata where
  parseJSON =
    Data.withObject
      "Ec2Metadata"
      ( \x ->
          Ec2Metadata'
            Prelude.<$> (x Data..:? "amiId")
            Prelude.<*> (x Data..:? "platform")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Ec2Metadata where
  hashWithSalt _salt Ec2Metadata' {..} =
    _salt
      `Prelude.hashWithSalt` amiId
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` tags

instance Prelude.NFData Ec2Metadata where
  rnf Ec2Metadata' {..} =
    Prelude.rnf amiId
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf tags

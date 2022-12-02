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
-- Module      : Amazonka.Snowball.Types.Ec2AmiResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.Ec2AmiResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A JSON-formatted object that contains the IDs for an Amazon Machine
-- Image (AMI), including the Amazon EC2 AMI ID and the Snow device AMI ID.
-- Each AMI has these two IDs to simplify identifying the AMI in both the
-- Amazon Web Services Cloud and on the device.
--
-- /See:/ 'newEc2AmiResource' smart constructor.
data Ec2AmiResource = Ec2AmiResource'
  { -- | The ID of the AMI on the Snow device.
    snowballAmiId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AMI in Amazon EC2.
    amiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ec2AmiResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snowballAmiId', 'ec2AmiResource_snowballAmiId' - The ID of the AMI on the Snow device.
--
-- 'amiId', 'ec2AmiResource_amiId' - The ID of the AMI in Amazon EC2.
newEc2AmiResource ::
  -- | 'amiId'
  Prelude.Text ->
  Ec2AmiResource
newEc2AmiResource pAmiId_ =
  Ec2AmiResource'
    { snowballAmiId = Prelude.Nothing,
      amiId = pAmiId_
    }

-- | The ID of the AMI on the Snow device.
ec2AmiResource_snowballAmiId :: Lens.Lens' Ec2AmiResource (Prelude.Maybe Prelude.Text)
ec2AmiResource_snowballAmiId = Lens.lens (\Ec2AmiResource' {snowballAmiId} -> snowballAmiId) (\s@Ec2AmiResource' {} a -> s {snowballAmiId = a} :: Ec2AmiResource)

-- | The ID of the AMI in Amazon EC2.
ec2AmiResource_amiId :: Lens.Lens' Ec2AmiResource Prelude.Text
ec2AmiResource_amiId = Lens.lens (\Ec2AmiResource' {amiId} -> amiId) (\s@Ec2AmiResource' {} a -> s {amiId = a} :: Ec2AmiResource)

instance Data.FromJSON Ec2AmiResource where
  parseJSON =
    Data.withObject
      "Ec2AmiResource"
      ( \x ->
          Ec2AmiResource'
            Prelude.<$> (x Data..:? "SnowballAmiId")
            Prelude.<*> (x Data..: "AmiId")
      )

instance Prelude.Hashable Ec2AmiResource where
  hashWithSalt _salt Ec2AmiResource' {..} =
    _salt `Prelude.hashWithSalt` snowballAmiId
      `Prelude.hashWithSalt` amiId

instance Prelude.NFData Ec2AmiResource where
  rnf Ec2AmiResource' {..} =
    Prelude.rnf snowballAmiId
      `Prelude.seq` Prelude.rnf amiId

instance Data.ToJSON Ec2AmiResource where
  toJSON Ec2AmiResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SnowballAmiId" Data..=) Prelude.<$> snowballAmiId,
            Prelude.Just ("AmiId" Data..= amiId)
          ]
      )

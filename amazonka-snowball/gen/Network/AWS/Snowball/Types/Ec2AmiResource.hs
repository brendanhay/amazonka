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
-- Module      : Network.AWS.Snowball.Types.Ec2AmiResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.Ec2AmiResource where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A JSON-formatted object that contains the IDs for an Amazon Machine
-- Image (AMI), including the Amazon EC2 AMI ID and the Snow device AMI ID.
-- Each AMI has these two IDs to simplify identifying the AMI in both the
-- AWS Cloud and on the device.
--
-- /See:/ 'newEc2AmiResource' smart constructor.
data Ec2AmiResource = Ec2AmiResource'
  { -- | The ID of the AMI on the Snow device.
    snowballAmiId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AMI in Amazon EC2.
    amiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON Ec2AmiResource where
  parseJSON =
    Prelude.withObject
      "Ec2AmiResource"
      ( \x ->
          Ec2AmiResource'
            Prelude.<$> (x Prelude..:? "SnowballAmiId")
            Prelude.<*> (x Prelude..: "AmiId")
      )

instance Prelude.Hashable Ec2AmiResource

instance Prelude.NFData Ec2AmiResource

instance Prelude.ToJSON Ec2AmiResource where
  toJSON Ec2AmiResource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SnowballAmiId" Prelude..=)
              Prelude.<$> snowballAmiId,
            Prelude.Just ("AmiId" Prelude..= amiId)
          ]
      )

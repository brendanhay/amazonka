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
-- Module      : Amazonka.ImageBuilder.Types.Ami
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.Ami where

import qualified Amazonka.Core as Core
import Amazonka.ImageBuilder.Types.ImageState
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details of an Amazon EC2 AMI.
--
-- /See:/ 'newAmi' smart constructor.
data Ami = Ami'
  { -- | The AMI ID of the Amazon EC2 AMI.
    image :: Prelude.Maybe Prelude.Text,
    state :: Prelude.Maybe ImageState,
    -- | The account ID of the owner of the AMI.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon EC2 AMI.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region of the Amazon EC2 AMI.
    region :: Prelude.Maybe Prelude.Text,
    -- | The description of the Amazon EC2 AMI. Minimum and maximum length are in
    -- characters.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ami' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'image', 'ami_image' - The AMI ID of the Amazon EC2 AMI.
--
-- 'state', 'ami_state' - Undocumented member.
--
-- 'accountId', 'ami_accountId' - The account ID of the owner of the AMI.
--
-- 'name', 'ami_name' - The name of the Amazon EC2 AMI.
--
-- 'region', 'ami_region' - The Amazon Web Services Region of the Amazon EC2 AMI.
--
-- 'description', 'ami_description' - The description of the Amazon EC2 AMI. Minimum and maximum length are in
-- characters.
newAmi ::
  Ami
newAmi =
  Ami'
    { image = Prelude.Nothing,
      state = Prelude.Nothing,
      accountId = Prelude.Nothing,
      name = Prelude.Nothing,
      region = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The AMI ID of the Amazon EC2 AMI.
ami_image :: Lens.Lens' Ami (Prelude.Maybe Prelude.Text)
ami_image = Lens.lens (\Ami' {image} -> image) (\s@Ami' {} a -> s {image = a} :: Ami)

-- | Undocumented member.
ami_state :: Lens.Lens' Ami (Prelude.Maybe ImageState)
ami_state = Lens.lens (\Ami' {state} -> state) (\s@Ami' {} a -> s {state = a} :: Ami)

-- | The account ID of the owner of the AMI.
ami_accountId :: Lens.Lens' Ami (Prelude.Maybe Prelude.Text)
ami_accountId = Lens.lens (\Ami' {accountId} -> accountId) (\s@Ami' {} a -> s {accountId = a} :: Ami)

-- | The name of the Amazon EC2 AMI.
ami_name :: Lens.Lens' Ami (Prelude.Maybe Prelude.Text)
ami_name = Lens.lens (\Ami' {name} -> name) (\s@Ami' {} a -> s {name = a} :: Ami)

-- | The Amazon Web Services Region of the Amazon EC2 AMI.
ami_region :: Lens.Lens' Ami (Prelude.Maybe Prelude.Text)
ami_region = Lens.lens (\Ami' {region} -> region) (\s@Ami' {} a -> s {region = a} :: Ami)

-- | The description of the Amazon EC2 AMI. Minimum and maximum length are in
-- characters.
ami_description :: Lens.Lens' Ami (Prelude.Maybe Prelude.Text)
ami_description = Lens.lens (\Ami' {description} -> description) (\s@Ami' {} a -> s {description = a} :: Ami)

instance Core.FromJSON Ami where
  parseJSON =
    Core.withObject
      "Ami"
      ( \x ->
          Ami'
            Prelude.<$> (x Core..:? "image")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "accountId")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "region")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable Ami where
  hashWithSalt _salt Ami' {..} =
    _salt `Prelude.hashWithSalt` image
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` description

instance Prelude.NFData Ami where
  rnf Ami' {..} =
    Prelude.rnf image
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf description

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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.Ami where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.ImageState
import qualified Amazonka.Prelude as Prelude

-- | Details of an Amazon EC2 AMI.
--
-- /See:/ 'newAmi' smart constructor.
data Ami = Ami'
  { -- | The account ID of the owner of the AMI.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The description of the Amazon EC2 AMI. Minimum and maximum length are in
    -- characters.
    description :: Prelude.Maybe Prelude.Text,
    -- | The AMI ID of the Amazon EC2 AMI.
    image :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon EC2 AMI.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region of the Amazon EC2 AMI.
    region :: Prelude.Maybe Prelude.Text,
    state :: Prelude.Maybe ImageState
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
-- 'accountId', 'ami_accountId' - The account ID of the owner of the AMI.
--
-- 'description', 'ami_description' - The description of the Amazon EC2 AMI. Minimum and maximum length are in
-- characters.
--
-- 'image', 'ami_image' - The AMI ID of the Amazon EC2 AMI.
--
-- 'name', 'ami_name' - The name of the Amazon EC2 AMI.
--
-- 'region', 'ami_region' - The Amazon Web Services Region of the Amazon EC2 AMI.
--
-- 'state', 'ami_state' - Undocumented member.
newAmi ::
  Ami
newAmi =
  Ami'
    { accountId = Prelude.Nothing,
      description = Prelude.Nothing,
      image = Prelude.Nothing,
      name = Prelude.Nothing,
      region = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The account ID of the owner of the AMI.
ami_accountId :: Lens.Lens' Ami (Prelude.Maybe Prelude.Text)
ami_accountId = Lens.lens (\Ami' {accountId} -> accountId) (\s@Ami' {} a -> s {accountId = a} :: Ami)

-- | The description of the Amazon EC2 AMI. Minimum and maximum length are in
-- characters.
ami_description :: Lens.Lens' Ami (Prelude.Maybe Prelude.Text)
ami_description = Lens.lens (\Ami' {description} -> description) (\s@Ami' {} a -> s {description = a} :: Ami)

-- | The AMI ID of the Amazon EC2 AMI.
ami_image :: Lens.Lens' Ami (Prelude.Maybe Prelude.Text)
ami_image = Lens.lens (\Ami' {image} -> image) (\s@Ami' {} a -> s {image = a} :: Ami)

-- | The name of the Amazon EC2 AMI.
ami_name :: Lens.Lens' Ami (Prelude.Maybe Prelude.Text)
ami_name = Lens.lens (\Ami' {name} -> name) (\s@Ami' {} a -> s {name = a} :: Ami)

-- | The Amazon Web Services Region of the Amazon EC2 AMI.
ami_region :: Lens.Lens' Ami (Prelude.Maybe Prelude.Text)
ami_region = Lens.lens (\Ami' {region} -> region) (\s@Ami' {} a -> s {region = a} :: Ami)

-- | Undocumented member.
ami_state :: Lens.Lens' Ami (Prelude.Maybe ImageState)
ami_state = Lens.lens (\Ami' {state} -> state) (\s@Ami' {} a -> s {state = a} :: Ami)

instance Data.FromJSON Ami where
  parseJSON =
    Data.withObject
      "Ami"
      ( \x ->
          Ami'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "image")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "region")
            Prelude.<*> (x Data..:? "state")
      )

instance Prelude.Hashable Ami where
  hashWithSalt _salt Ami' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` image
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` state

instance Prelude.NFData Ami where
  rnf Ami' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf image
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf state

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
-- Module      : Amazonka.NetworkManager.Types.VpcAttachment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.VpcAttachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.Attachment
import Amazonka.NetworkManager.Types.VpcOptions
import qualified Amazonka.Prelude as Prelude

-- | Describes a VPC attachment.
--
-- /See:/ 'newVpcAttachment' smart constructor.
data VpcAttachment = VpcAttachment'
  { -- | Provides details about the VPC attachment.
    attachment :: Prelude.Maybe Attachment,
    -- | The subnet ARNs.
    subnetArns :: Prelude.Maybe [Prelude.Text],
    -- | Provides details about the VPC attachment.
    options :: Prelude.Maybe VpcOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachment', 'vpcAttachment_attachment' - Provides details about the VPC attachment.
--
-- 'subnetArns', 'vpcAttachment_subnetArns' - The subnet ARNs.
--
-- 'options', 'vpcAttachment_options' - Provides details about the VPC attachment.
newVpcAttachment ::
  VpcAttachment
newVpcAttachment =
  VpcAttachment'
    { attachment = Prelude.Nothing,
      subnetArns = Prelude.Nothing,
      options = Prelude.Nothing
    }

-- | Provides details about the VPC attachment.
vpcAttachment_attachment :: Lens.Lens' VpcAttachment (Prelude.Maybe Attachment)
vpcAttachment_attachment = Lens.lens (\VpcAttachment' {attachment} -> attachment) (\s@VpcAttachment' {} a -> s {attachment = a} :: VpcAttachment)

-- | The subnet ARNs.
vpcAttachment_subnetArns :: Lens.Lens' VpcAttachment (Prelude.Maybe [Prelude.Text])
vpcAttachment_subnetArns = Lens.lens (\VpcAttachment' {subnetArns} -> subnetArns) (\s@VpcAttachment' {} a -> s {subnetArns = a} :: VpcAttachment) Prelude.. Lens.mapping Lens.coerced

-- | Provides details about the VPC attachment.
vpcAttachment_options :: Lens.Lens' VpcAttachment (Prelude.Maybe VpcOptions)
vpcAttachment_options = Lens.lens (\VpcAttachment' {options} -> options) (\s@VpcAttachment' {} a -> s {options = a} :: VpcAttachment)

instance Data.FromJSON VpcAttachment where
  parseJSON =
    Data.withObject
      "VpcAttachment"
      ( \x ->
          VpcAttachment'
            Prelude.<$> (x Data..:? "Attachment")
            Prelude.<*> (x Data..:? "SubnetArns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Options")
      )

instance Prelude.Hashable VpcAttachment where
  hashWithSalt _salt VpcAttachment' {..} =
    _salt `Prelude.hashWithSalt` attachment
      `Prelude.hashWithSalt` subnetArns
      `Prelude.hashWithSalt` options

instance Prelude.NFData VpcAttachment where
  rnf VpcAttachment' {..} =
    Prelude.rnf attachment
      `Prelude.seq` Prelude.rnf subnetArns
      `Prelude.seq` Prelude.rnf options

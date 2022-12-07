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
-- Module      : Amazonka.PinpointEmail.Types.Destination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointEmail.Types.Destination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that describes the recipients for an email.
--
-- /See:/ 'newDestination' smart constructor.
data Destination = Destination'
  { -- | An array that contains the email addresses of the \"CC\" (carbon copy)
    -- recipients for the email.
    ccAddresses :: Prelude.Maybe [Prelude.Text],
    -- | An array that contains the email addresses of the \"BCC\" (blind carbon
    -- copy) recipients for the email.
    bccAddresses :: Prelude.Maybe [Prelude.Text],
    -- | An array that contains the email addresses of the \"To\" recipients for
    -- the email.
    toAddresses :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Destination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ccAddresses', 'destination_ccAddresses' - An array that contains the email addresses of the \"CC\" (carbon copy)
-- recipients for the email.
--
-- 'bccAddresses', 'destination_bccAddresses' - An array that contains the email addresses of the \"BCC\" (blind carbon
-- copy) recipients for the email.
--
-- 'toAddresses', 'destination_toAddresses' - An array that contains the email addresses of the \"To\" recipients for
-- the email.
newDestination ::
  Destination
newDestination =
  Destination'
    { ccAddresses = Prelude.Nothing,
      bccAddresses = Prelude.Nothing,
      toAddresses = Prelude.Nothing
    }

-- | An array that contains the email addresses of the \"CC\" (carbon copy)
-- recipients for the email.
destination_ccAddresses :: Lens.Lens' Destination (Prelude.Maybe [Prelude.Text])
destination_ccAddresses = Lens.lens (\Destination' {ccAddresses} -> ccAddresses) (\s@Destination' {} a -> s {ccAddresses = a} :: Destination) Prelude.. Lens.mapping Lens.coerced

-- | An array that contains the email addresses of the \"BCC\" (blind carbon
-- copy) recipients for the email.
destination_bccAddresses :: Lens.Lens' Destination (Prelude.Maybe [Prelude.Text])
destination_bccAddresses = Lens.lens (\Destination' {bccAddresses} -> bccAddresses) (\s@Destination' {} a -> s {bccAddresses = a} :: Destination) Prelude.. Lens.mapping Lens.coerced

-- | An array that contains the email addresses of the \"To\" recipients for
-- the email.
destination_toAddresses :: Lens.Lens' Destination (Prelude.Maybe [Prelude.Text])
destination_toAddresses = Lens.lens (\Destination' {toAddresses} -> toAddresses) (\s@Destination' {} a -> s {toAddresses = a} :: Destination) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable Destination where
  hashWithSalt _salt Destination' {..} =
    _salt `Prelude.hashWithSalt` ccAddresses
      `Prelude.hashWithSalt` bccAddresses
      `Prelude.hashWithSalt` toAddresses

instance Prelude.NFData Destination where
  rnf Destination' {..} =
    Prelude.rnf ccAddresses
      `Prelude.seq` Prelude.rnf bccAddresses
      `Prelude.seq` Prelude.rnf toAddresses

instance Data.ToJSON Destination where
  toJSON Destination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CcAddresses" Data..=) Prelude.<$> ccAddresses,
            ("BccAddresses" Data..=) Prelude.<$> bccAddresses,
            ("ToAddresses" Data..=) Prelude.<$> toAddresses
          ]
      )

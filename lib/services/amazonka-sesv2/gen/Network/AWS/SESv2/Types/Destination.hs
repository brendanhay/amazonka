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
-- Module      : Network.AWS.SESv2.Types.Destination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.Destination where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that describes the recipients for an email.
--
-- /See:/ 'newDestination' smart constructor.
data Destination = Destination'
  { -- | An array that contains the email addresses of the \"To\" recipients for
    -- the email.
    toAddresses :: Prelude.Maybe [Prelude.Text],
    -- | An array that contains the email addresses of the \"CC\" (carbon copy)
    -- recipients for the email.
    ccAddresses :: Prelude.Maybe [Prelude.Text],
    -- | An array that contains the email addresses of the \"BCC\" (blind carbon
    -- copy) recipients for the email.
    bccAddresses :: Prelude.Maybe [Prelude.Text]
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
-- 'toAddresses', 'destination_toAddresses' - An array that contains the email addresses of the \"To\" recipients for
-- the email.
--
-- 'ccAddresses', 'destination_ccAddresses' - An array that contains the email addresses of the \"CC\" (carbon copy)
-- recipients for the email.
--
-- 'bccAddresses', 'destination_bccAddresses' - An array that contains the email addresses of the \"BCC\" (blind carbon
-- copy) recipients for the email.
newDestination ::
  Destination
newDestination =
  Destination'
    { toAddresses = Prelude.Nothing,
      ccAddresses = Prelude.Nothing,
      bccAddresses = Prelude.Nothing
    }

-- | An array that contains the email addresses of the \"To\" recipients for
-- the email.
destination_toAddresses :: Lens.Lens' Destination (Prelude.Maybe [Prelude.Text])
destination_toAddresses = Lens.lens (\Destination' {toAddresses} -> toAddresses) (\s@Destination' {} a -> s {toAddresses = a} :: Destination) Prelude.. Lens.mapping Lens._Coerce

-- | An array that contains the email addresses of the \"CC\" (carbon copy)
-- recipients for the email.
destination_ccAddresses :: Lens.Lens' Destination (Prelude.Maybe [Prelude.Text])
destination_ccAddresses = Lens.lens (\Destination' {ccAddresses} -> ccAddresses) (\s@Destination' {} a -> s {ccAddresses = a} :: Destination) Prelude.. Lens.mapping Lens._Coerce

-- | An array that contains the email addresses of the \"BCC\" (blind carbon
-- copy) recipients for the email.
destination_bccAddresses :: Lens.Lens' Destination (Prelude.Maybe [Prelude.Text])
destination_bccAddresses = Lens.lens (\Destination' {bccAddresses} -> bccAddresses) (\s@Destination' {} a -> s {bccAddresses = a} :: Destination) Prelude.. Lens.mapping Lens._Coerce

instance Prelude.Hashable Destination

instance Prelude.NFData Destination

instance Core.ToJSON Destination where
  toJSON Destination' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ToAddresses" Core..=) Prelude.<$> toAddresses,
            ("CcAddresses" Core..=) Prelude.<$> ccAddresses,
            ("BccAddresses" Core..=) Prelude.<$> bccAddresses
          ]
      )

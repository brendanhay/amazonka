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
-- Module      : Amazonka.EC2.Types.AttachmentEnaSrdSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AttachmentEnaSrdSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AttachmentEnaSrdUdpSpecification
import qualified Amazonka.Prelude as Prelude

-- | Describes the ENA Express configuration for the network interface
-- that\'s attached to the instance.
--
-- /See:/ 'newAttachmentEnaSrdSpecification' smart constructor.
data AttachmentEnaSrdSpecification = AttachmentEnaSrdSpecification'
  { -- | Indicates whether ENA Express is enabled for the network interface
    -- that\'s attached to the instance.
    enaSrdEnabled :: Prelude.Maybe Prelude.Bool,
    -- | ENA Express configuration for UDP network traffic.
    enaSrdUdpSpecification :: Prelude.Maybe AttachmentEnaSrdUdpSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachmentEnaSrdSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enaSrdEnabled', 'attachmentEnaSrdSpecification_enaSrdEnabled' - Indicates whether ENA Express is enabled for the network interface
-- that\'s attached to the instance.
--
-- 'enaSrdUdpSpecification', 'attachmentEnaSrdSpecification_enaSrdUdpSpecification' - ENA Express configuration for UDP network traffic.
newAttachmentEnaSrdSpecification ::
  AttachmentEnaSrdSpecification
newAttachmentEnaSrdSpecification =
  AttachmentEnaSrdSpecification'
    { enaSrdEnabled =
        Prelude.Nothing,
      enaSrdUdpSpecification = Prelude.Nothing
    }

-- | Indicates whether ENA Express is enabled for the network interface
-- that\'s attached to the instance.
attachmentEnaSrdSpecification_enaSrdEnabled :: Lens.Lens' AttachmentEnaSrdSpecification (Prelude.Maybe Prelude.Bool)
attachmentEnaSrdSpecification_enaSrdEnabled = Lens.lens (\AttachmentEnaSrdSpecification' {enaSrdEnabled} -> enaSrdEnabled) (\s@AttachmentEnaSrdSpecification' {} a -> s {enaSrdEnabled = a} :: AttachmentEnaSrdSpecification)

-- | ENA Express configuration for UDP network traffic.
attachmentEnaSrdSpecification_enaSrdUdpSpecification :: Lens.Lens' AttachmentEnaSrdSpecification (Prelude.Maybe AttachmentEnaSrdUdpSpecification)
attachmentEnaSrdSpecification_enaSrdUdpSpecification = Lens.lens (\AttachmentEnaSrdSpecification' {enaSrdUdpSpecification} -> enaSrdUdpSpecification) (\s@AttachmentEnaSrdSpecification' {} a -> s {enaSrdUdpSpecification = a} :: AttachmentEnaSrdSpecification)

instance Data.FromXML AttachmentEnaSrdSpecification where
  parseXML x =
    AttachmentEnaSrdSpecification'
      Prelude.<$> (x Data..@? "enaSrdEnabled")
      Prelude.<*> (x Data..@? "enaSrdUdpSpecification")

instance
  Prelude.Hashable
    AttachmentEnaSrdSpecification
  where
  hashWithSalt _salt AttachmentEnaSrdSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` enaSrdEnabled
      `Prelude.hashWithSalt` enaSrdUdpSpecification

instance Prelude.NFData AttachmentEnaSrdSpecification where
  rnf AttachmentEnaSrdSpecification' {..} =
    Prelude.rnf enaSrdEnabled `Prelude.seq`
      Prelude.rnf enaSrdUdpSpecification

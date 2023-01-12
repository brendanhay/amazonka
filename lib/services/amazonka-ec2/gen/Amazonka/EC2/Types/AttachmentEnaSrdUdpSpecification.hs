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
-- Module      : Amazonka.EC2.Types.AttachmentEnaSrdUdpSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AttachmentEnaSrdUdpSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the ENA Express configuration for UDP traffic on the network
-- interface that\'s attached to the instance.
--
-- /See:/ 'newAttachmentEnaSrdUdpSpecification' smart constructor.
data AttachmentEnaSrdUdpSpecification = AttachmentEnaSrdUdpSpecification'
  { -- | Indicates whether UDP traffic to and from the instance uses ENA Express.
    -- To specify this setting, you must first enable ENA Express.
    enaSrdUdpEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachmentEnaSrdUdpSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enaSrdUdpEnabled', 'attachmentEnaSrdUdpSpecification_enaSrdUdpEnabled' - Indicates whether UDP traffic to and from the instance uses ENA Express.
-- To specify this setting, you must first enable ENA Express.
newAttachmentEnaSrdUdpSpecification ::
  AttachmentEnaSrdUdpSpecification
newAttachmentEnaSrdUdpSpecification =
  AttachmentEnaSrdUdpSpecification'
    { enaSrdUdpEnabled =
        Prelude.Nothing
    }

-- | Indicates whether UDP traffic to and from the instance uses ENA Express.
-- To specify this setting, you must first enable ENA Express.
attachmentEnaSrdUdpSpecification_enaSrdUdpEnabled :: Lens.Lens' AttachmentEnaSrdUdpSpecification (Prelude.Maybe Prelude.Bool)
attachmentEnaSrdUdpSpecification_enaSrdUdpEnabled = Lens.lens (\AttachmentEnaSrdUdpSpecification' {enaSrdUdpEnabled} -> enaSrdUdpEnabled) (\s@AttachmentEnaSrdUdpSpecification' {} a -> s {enaSrdUdpEnabled = a} :: AttachmentEnaSrdUdpSpecification)

instance
  Data.FromXML
    AttachmentEnaSrdUdpSpecification
  where
  parseXML x =
    AttachmentEnaSrdUdpSpecification'
      Prelude.<$> (x Data..@? "enaSrdUdpEnabled")

instance
  Prelude.Hashable
    AttachmentEnaSrdUdpSpecification
  where
  hashWithSalt
    _salt
    AttachmentEnaSrdUdpSpecification' {..} =
      _salt `Prelude.hashWithSalt` enaSrdUdpEnabled

instance
  Prelude.NFData
    AttachmentEnaSrdUdpSpecification
  where
  rnf AttachmentEnaSrdUdpSpecification' {..} =
    Prelude.rnf enaSrdUdpEnabled

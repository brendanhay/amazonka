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
-- Module      : Amazonka.EC2.Types.PeeringAttachmentStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PeeringAttachmentStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The status of the transit gateway peering attachment.
--
-- /See:/ 'newPeeringAttachmentStatus' smart constructor.
data PeeringAttachmentStatus = PeeringAttachmentStatus'
  { -- | The status code.
    code :: Prelude.Maybe Prelude.Text,
    -- | The status message, if applicable.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PeeringAttachmentStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'peeringAttachmentStatus_code' - The status code.
--
-- 'message', 'peeringAttachmentStatus_message' - The status message, if applicable.
newPeeringAttachmentStatus ::
  PeeringAttachmentStatus
newPeeringAttachmentStatus =
  PeeringAttachmentStatus'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The status code.
peeringAttachmentStatus_code :: Lens.Lens' PeeringAttachmentStatus (Prelude.Maybe Prelude.Text)
peeringAttachmentStatus_code = Lens.lens (\PeeringAttachmentStatus' {code} -> code) (\s@PeeringAttachmentStatus' {} a -> s {code = a} :: PeeringAttachmentStatus)

-- | The status message, if applicable.
peeringAttachmentStatus_message :: Lens.Lens' PeeringAttachmentStatus (Prelude.Maybe Prelude.Text)
peeringAttachmentStatus_message = Lens.lens (\PeeringAttachmentStatus' {message} -> message) (\s@PeeringAttachmentStatus' {} a -> s {message = a} :: PeeringAttachmentStatus)

instance Data.FromXML PeeringAttachmentStatus where
  parseXML x =
    PeeringAttachmentStatus'
      Prelude.<$> (x Data..@? "code")
      Prelude.<*> (x Data..@? "message")

instance Prelude.Hashable PeeringAttachmentStatus where
  hashWithSalt _salt PeeringAttachmentStatus' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData PeeringAttachmentStatus where
  rnf PeeringAttachmentStatus' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message

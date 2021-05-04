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
-- Module      : Network.AWS.EC2.Types.PeeringAttachmentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PeeringAttachmentStatus where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The status of the transit gateway peering attachment.
--
-- /See:/ 'newPeeringAttachmentStatus' smart constructor.
data PeeringAttachmentStatus = PeeringAttachmentStatus'
  { -- | The status message, if applicable.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status code.
    code :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PeeringAttachmentStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'peeringAttachmentStatus_message' - The status message, if applicable.
--
-- 'code', 'peeringAttachmentStatus_code' - The status code.
newPeeringAttachmentStatus ::
  PeeringAttachmentStatus
newPeeringAttachmentStatus =
  PeeringAttachmentStatus'
    { message = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The status message, if applicable.
peeringAttachmentStatus_message :: Lens.Lens' PeeringAttachmentStatus (Prelude.Maybe Prelude.Text)
peeringAttachmentStatus_message = Lens.lens (\PeeringAttachmentStatus' {message} -> message) (\s@PeeringAttachmentStatus' {} a -> s {message = a} :: PeeringAttachmentStatus)

-- | The status code.
peeringAttachmentStatus_code :: Lens.Lens' PeeringAttachmentStatus (Prelude.Maybe Prelude.Text)
peeringAttachmentStatus_code = Lens.lens (\PeeringAttachmentStatus' {code} -> code) (\s@PeeringAttachmentStatus' {} a -> s {code = a} :: PeeringAttachmentStatus)

instance Prelude.FromXML PeeringAttachmentStatus where
  parseXML x =
    PeeringAttachmentStatus'
      Prelude.<$> (x Prelude..@? "message")
      Prelude.<*> (x Prelude..@? "code")

instance Prelude.Hashable PeeringAttachmentStatus

instance Prelude.NFData PeeringAttachmentStatus

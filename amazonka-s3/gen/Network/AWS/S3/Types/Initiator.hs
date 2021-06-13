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
-- Module      : Network.AWS.S3.Types.Initiator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Initiator where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

-- | Container element that identifies who initiated the multipart upload.
--
-- /See:/ 'newInitiator' smart constructor.
data Initiator = Initiator'
  { -- | If the principal is an AWS account, it provides the Canonical User ID.
    -- If the principal is an IAM User, it provides a user ARN value.
    id :: Prelude.Maybe Prelude.Text,
    -- | Name of the Principal.
    displayName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Initiator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'initiator_id' - If the principal is an AWS account, it provides the Canonical User ID.
-- If the principal is an IAM User, it provides a user ARN value.
--
-- 'displayName', 'initiator_displayName' - Name of the Principal.
newInitiator ::
  Initiator
newInitiator =
  Initiator'
    { id = Prelude.Nothing,
      displayName = Prelude.Nothing
    }

-- | If the principal is an AWS account, it provides the Canonical User ID.
-- If the principal is an IAM User, it provides a user ARN value.
initiator_id :: Lens.Lens' Initiator (Prelude.Maybe Prelude.Text)
initiator_id = Lens.lens (\Initiator' {id} -> id) (\s@Initiator' {} a -> s {id = a} :: Initiator)

-- | Name of the Principal.
initiator_displayName :: Lens.Lens' Initiator (Prelude.Maybe Prelude.Text)
initiator_displayName = Lens.lens (\Initiator' {displayName} -> displayName) (\s@Initiator' {} a -> s {displayName = a} :: Initiator)

instance Core.FromXML Initiator where
  parseXML x =
    Initiator'
      Prelude.<$> (x Core..@? "ID")
      Prelude.<*> (x Core..@? "DisplayName")

instance Prelude.Hashable Initiator

instance Prelude.NFData Initiator

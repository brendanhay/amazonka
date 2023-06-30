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
-- Module      : Amazonka.S3.Types.Initiator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.Initiator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

-- | Container element that identifies who initiated the multipart upload.
--
-- /See:/ 'newInitiator' smart constructor.
data Initiator = Initiator'
  { -- | Name of the Principal.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | If the principal is an Amazon Web Services account, it provides the
    -- Canonical User ID. If the principal is an IAM User, it provides a user
    -- ARN value.
    id :: Prelude.Maybe Prelude.Text
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
-- 'displayName', 'initiator_displayName' - Name of the Principal.
--
-- 'id', 'initiator_id' - If the principal is an Amazon Web Services account, it provides the
-- Canonical User ID. If the principal is an IAM User, it provides a user
-- ARN value.
newInitiator ::
  Initiator
newInitiator =
  Initiator'
    { displayName = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | Name of the Principal.
initiator_displayName :: Lens.Lens' Initiator (Prelude.Maybe Prelude.Text)
initiator_displayName = Lens.lens (\Initiator' {displayName} -> displayName) (\s@Initiator' {} a -> s {displayName = a} :: Initiator)

-- | If the principal is an Amazon Web Services account, it provides the
-- Canonical User ID. If the principal is an IAM User, it provides a user
-- ARN value.
initiator_id :: Lens.Lens' Initiator (Prelude.Maybe Prelude.Text)
initiator_id = Lens.lens (\Initiator' {id} -> id) (\s@Initiator' {} a -> s {id = a} :: Initiator)

instance Data.FromXML Initiator where
  parseXML x =
    Initiator'
      Prelude.<$> (x Data..@? "DisplayName")
      Prelude.<*> (x Data..@? "ID")

instance Prelude.Hashable Initiator where
  hashWithSalt _salt Initiator' {..} =
    _salt
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` id

instance Prelude.NFData Initiator where
  rnf Initiator' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf id

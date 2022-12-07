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
-- Module      : Amazonka.DrS.Types.StagingArea
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.StagingArea where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types.ExtensionStatus
import qualified Amazonka.Prelude as Prelude

-- | Staging information related to source server.
--
-- /See:/ 'newStagingArea' smart constructor.
data StagingArea = StagingArea'
  { -- | Account ID of the account to which source server belongs. If this source
    -- server is extended - shows Account ID of staging source server.
    stagingAccountID :: Prelude.Maybe Prelude.Text,
    -- | Shows an error message that occurred when DRS tried to access the
    -- staging source server. In this case StagingArea$status will have value
    -- EXTENSION_ERROR
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Arn of the staging source server if this source server is extended
    stagingSourceServerArn :: Prelude.Maybe Prelude.Text,
    -- | Status of Source server extension. Possible values: (a) NOT_EXTENDED -
    -- This is a source server that is replicating in the current account. (b)
    -- EXTENDED - Source server is extended from a staging source server. In
    -- this case, the value of stagingSourceServerArn is pointing to the Arn of
    -- the source server in the staging account. (c) EXTENSION_ERROR - Some
    -- issue occurred when accessing staging source server. In this case,
    -- errorMessage field will contain an error message that explains what
    -- happened.
    status :: Prelude.Maybe ExtensionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StagingArea' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stagingAccountID', 'stagingArea_stagingAccountID' - Account ID of the account to which source server belongs. If this source
-- server is extended - shows Account ID of staging source server.
--
-- 'errorMessage', 'stagingArea_errorMessage' - Shows an error message that occurred when DRS tried to access the
-- staging source server. In this case StagingArea$status will have value
-- EXTENSION_ERROR
--
-- 'stagingSourceServerArn', 'stagingArea_stagingSourceServerArn' - Arn of the staging source server if this source server is extended
--
-- 'status', 'stagingArea_status' - Status of Source server extension. Possible values: (a) NOT_EXTENDED -
-- This is a source server that is replicating in the current account. (b)
-- EXTENDED - Source server is extended from a staging source server. In
-- this case, the value of stagingSourceServerArn is pointing to the Arn of
-- the source server in the staging account. (c) EXTENSION_ERROR - Some
-- issue occurred when accessing staging source server. In this case,
-- errorMessage field will contain an error message that explains what
-- happened.
newStagingArea ::
  StagingArea
newStagingArea =
  StagingArea'
    { stagingAccountID = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      stagingSourceServerArn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Account ID of the account to which source server belongs. If this source
-- server is extended - shows Account ID of staging source server.
stagingArea_stagingAccountID :: Lens.Lens' StagingArea (Prelude.Maybe Prelude.Text)
stagingArea_stagingAccountID = Lens.lens (\StagingArea' {stagingAccountID} -> stagingAccountID) (\s@StagingArea' {} a -> s {stagingAccountID = a} :: StagingArea)

-- | Shows an error message that occurred when DRS tried to access the
-- staging source server. In this case StagingArea$status will have value
-- EXTENSION_ERROR
stagingArea_errorMessage :: Lens.Lens' StagingArea (Prelude.Maybe Prelude.Text)
stagingArea_errorMessage = Lens.lens (\StagingArea' {errorMessage} -> errorMessage) (\s@StagingArea' {} a -> s {errorMessage = a} :: StagingArea)

-- | Arn of the staging source server if this source server is extended
stagingArea_stagingSourceServerArn :: Lens.Lens' StagingArea (Prelude.Maybe Prelude.Text)
stagingArea_stagingSourceServerArn = Lens.lens (\StagingArea' {stagingSourceServerArn} -> stagingSourceServerArn) (\s@StagingArea' {} a -> s {stagingSourceServerArn = a} :: StagingArea)

-- | Status of Source server extension. Possible values: (a) NOT_EXTENDED -
-- This is a source server that is replicating in the current account. (b)
-- EXTENDED - Source server is extended from a staging source server. In
-- this case, the value of stagingSourceServerArn is pointing to the Arn of
-- the source server in the staging account. (c) EXTENSION_ERROR - Some
-- issue occurred when accessing staging source server. In this case,
-- errorMessage field will contain an error message that explains what
-- happened.
stagingArea_status :: Lens.Lens' StagingArea (Prelude.Maybe ExtensionStatus)
stagingArea_status = Lens.lens (\StagingArea' {status} -> status) (\s@StagingArea' {} a -> s {status = a} :: StagingArea)

instance Data.FromJSON StagingArea where
  parseJSON =
    Data.withObject
      "StagingArea"
      ( \x ->
          StagingArea'
            Prelude.<$> (x Data..:? "stagingAccountID")
            Prelude.<*> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "stagingSourceServerArn")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable StagingArea where
  hashWithSalt _salt StagingArea' {..} =
    _salt `Prelude.hashWithSalt` stagingAccountID
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` stagingSourceServerArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData StagingArea where
  rnf StagingArea' {..} =
    Prelude.rnf stagingAccountID
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf stagingSourceServerArn
      `Prelude.seq` Prelude.rnf status

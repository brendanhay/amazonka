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
-- Module      : Amazonka.CloudWatch.Types.DashboardValidationMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.DashboardValidationMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An error or warning for the operation.
--
-- /See:/ 'newDashboardValidationMessage' smart constructor.
data DashboardValidationMessage = DashboardValidationMessage'
  { -- | The data path related to the message.
    dataPath :: Prelude.Maybe Prelude.Text,
    -- | A message describing the error or warning.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DashboardValidationMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataPath', 'dashboardValidationMessage_dataPath' - The data path related to the message.
--
-- 'message', 'dashboardValidationMessage_message' - A message describing the error or warning.
newDashboardValidationMessage ::
  DashboardValidationMessage
newDashboardValidationMessage =
  DashboardValidationMessage'
    { dataPath =
        Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The data path related to the message.
dashboardValidationMessage_dataPath :: Lens.Lens' DashboardValidationMessage (Prelude.Maybe Prelude.Text)
dashboardValidationMessage_dataPath = Lens.lens (\DashboardValidationMessage' {dataPath} -> dataPath) (\s@DashboardValidationMessage' {} a -> s {dataPath = a} :: DashboardValidationMessage)

-- | A message describing the error or warning.
dashboardValidationMessage_message :: Lens.Lens' DashboardValidationMessage (Prelude.Maybe Prelude.Text)
dashboardValidationMessage_message = Lens.lens (\DashboardValidationMessage' {message} -> message) (\s@DashboardValidationMessage' {} a -> s {message = a} :: DashboardValidationMessage)

instance Core.FromXML DashboardValidationMessage where
  parseXML x =
    DashboardValidationMessage'
      Prelude.<$> (x Core..@? "DataPath")
      Prelude.<*> (x Core..@? "Message")

instance Prelude.Hashable DashboardValidationMessage

instance Prelude.NFData DashboardValidationMessage

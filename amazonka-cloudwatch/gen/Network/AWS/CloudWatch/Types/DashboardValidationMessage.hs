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
-- Module      : Network.AWS.CloudWatch.Types.DashboardValidationMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.DashboardValidationMessage where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An error or warning for the operation.
--
-- /See:/ 'newDashboardValidationMessage' smart constructor.
data DashboardValidationMessage = DashboardValidationMessage'
  { -- | A message describing the error or warning.
    message :: Prelude.Maybe Prelude.Text,
    -- | The data path related to the message.
    dataPath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DashboardValidationMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'dashboardValidationMessage_message' - A message describing the error or warning.
--
-- 'dataPath', 'dashboardValidationMessage_dataPath' - The data path related to the message.
newDashboardValidationMessage ::
  DashboardValidationMessage
newDashboardValidationMessage =
  DashboardValidationMessage'
    { message =
        Prelude.Nothing,
      dataPath = Prelude.Nothing
    }

-- | A message describing the error or warning.
dashboardValidationMessage_message :: Lens.Lens' DashboardValidationMessage (Prelude.Maybe Prelude.Text)
dashboardValidationMessage_message = Lens.lens (\DashboardValidationMessage' {message} -> message) (\s@DashboardValidationMessage' {} a -> s {message = a} :: DashboardValidationMessage)

-- | The data path related to the message.
dashboardValidationMessage_dataPath :: Lens.Lens' DashboardValidationMessage (Prelude.Maybe Prelude.Text)
dashboardValidationMessage_dataPath = Lens.lens (\DashboardValidationMessage' {dataPath} -> dataPath) (\s@DashboardValidationMessage' {} a -> s {dataPath = a} :: DashboardValidationMessage)

instance Prelude.FromXML DashboardValidationMessage where
  parseXML x =
    DashboardValidationMessage'
      Prelude.<$> (x Prelude..@? "Message")
      Prelude.<*> (x Prelude..@? "DataPath")

instance Prelude.Hashable DashboardValidationMessage

instance Prelude.NFData DashboardValidationMessage

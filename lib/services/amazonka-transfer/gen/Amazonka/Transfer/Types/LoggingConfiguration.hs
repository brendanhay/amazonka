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
-- Module      : Amazonka.Transfer.Types.LoggingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.LoggingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Consists of the logging role and the log group name.
--
-- /See:/ 'newLoggingConfiguration' smart constructor.
data LoggingConfiguration = LoggingConfiguration'
  { -- | The name of the CloudWatch logging group for the Transfer Family server
    -- to which this workflow belongs.
    logGroupName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) role that allows a server to turn on Amazon CloudWatch logging for
    -- Amazon S3 or Amazon EFSevents. When set, you can view user activity in
    -- your CloudWatch logs.
    loggingRole :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupName', 'loggingConfiguration_logGroupName' - The name of the CloudWatch logging group for the Transfer Family server
-- to which this workflow belongs.
--
-- 'loggingRole', 'loggingConfiguration_loggingRole' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that allows a server to turn on Amazon CloudWatch logging for
-- Amazon S3 or Amazon EFSevents. When set, you can view user activity in
-- your CloudWatch logs.
newLoggingConfiguration ::
  LoggingConfiguration
newLoggingConfiguration =
  LoggingConfiguration'
    { logGroupName =
        Prelude.Nothing,
      loggingRole = Prelude.Nothing
    }

-- | The name of the CloudWatch logging group for the Transfer Family server
-- to which this workflow belongs.
loggingConfiguration_logGroupName :: Lens.Lens' LoggingConfiguration (Prelude.Maybe Prelude.Text)
loggingConfiguration_logGroupName = Lens.lens (\LoggingConfiguration' {logGroupName} -> logGroupName) (\s@LoggingConfiguration' {} a -> s {logGroupName = a} :: LoggingConfiguration)

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that allows a server to turn on Amazon CloudWatch logging for
-- Amazon S3 or Amazon EFSevents. When set, you can view user activity in
-- your CloudWatch logs.
loggingConfiguration_loggingRole :: Lens.Lens' LoggingConfiguration (Prelude.Maybe Prelude.Text)
loggingConfiguration_loggingRole = Lens.lens (\LoggingConfiguration' {loggingRole} -> loggingRole) (\s@LoggingConfiguration' {} a -> s {loggingRole = a} :: LoggingConfiguration)

instance Data.FromJSON LoggingConfiguration where
  parseJSON =
    Data.withObject
      "LoggingConfiguration"
      ( \x ->
          LoggingConfiguration'
            Prelude.<$> (x Data..:? "LogGroupName")
            Prelude.<*> (x Data..:? "LoggingRole")
      )

instance Prelude.Hashable LoggingConfiguration where
  hashWithSalt _salt LoggingConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` loggingRole

instance Prelude.NFData LoggingConfiguration where
  rnf LoggingConfiguration' {..} =
    Prelude.rnf logGroupName `Prelude.seq`
      Prelude.rnf loggingRole

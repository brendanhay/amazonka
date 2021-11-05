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
-- Module      : Network.AWS.Transfer.Types.LoggingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transfer.Types.LoggingConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Consists of the logging role and the log group name.
--
-- /See:/ 'newLoggingConfiguration' smart constructor.
data LoggingConfiguration = LoggingConfiguration'
  { -- | Specifies the Amazon Resource Name (ARN) of the Amazon Web Services
    -- Identity and Access Management (IAM) role that allows a server to turn
    -- on Amazon CloudWatch logging for Amazon S3 or Amazon EFS events. When
    -- set, user activity can be viewed in your CloudWatch logs.
    loggingRole :: Prelude.Maybe Prelude.Text,
    -- | The name of the CloudWatch logging group for the Amazon Web Services
    -- Transfer server to which this workflow belongs.
    logGroupName :: Prelude.Maybe Prelude.Text
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
-- 'loggingRole', 'loggingConfiguration_loggingRole' - Specifies the Amazon Resource Name (ARN) of the Amazon Web Services
-- Identity and Access Management (IAM) role that allows a server to turn
-- on Amazon CloudWatch logging for Amazon S3 or Amazon EFS events. When
-- set, user activity can be viewed in your CloudWatch logs.
--
-- 'logGroupName', 'loggingConfiguration_logGroupName' - The name of the CloudWatch logging group for the Amazon Web Services
-- Transfer server to which this workflow belongs.
newLoggingConfiguration ::
  LoggingConfiguration
newLoggingConfiguration =
  LoggingConfiguration'
    { loggingRole =
        Prelude.Nothing,
      logGroupName = Prelude.Nothing
    }

-- | Specifies the Amazon Resource Name (ARN) of the Amazon Web Services
-- Identity and Access Management (IAM) role that allows a server to turn
-- on Amazon CloudWatch logging for Amazon S3 or Amazon EFS events. When
-- set, user activity can be viewed in your CloudWatch logs.
loggingConfiguration_loggingRole :: Lens.Lens' LoggingConfiguration (Prelude.Maybe Prelude.Text)
loggingConfiguration_loggingRole = Lens.lens (\LoggingConfiguration' {loggingRole} -> loggingRole) (\s@LoggingConfiguration' {} a -> s {loggingRole = a} :: LoggingConfiguration)

-- | The name of the CloudWatch logging group for the Amazon Web Services
-- Transfer server to which this workflow belongs.
loggingConfiguration_logGroupName :: Lens.Lens' LoggingConfiguration (Prelude.Maybe Prelude.Text)
loggingConfiguration_logGroupName = Lens.lens (\LoggingConfiguration' {logGroupName} -> logGroupName) (\s@LoggingConfiguration' {} a -> s {logGroupName = a} :: LoggingConfiguration)

instance Core.FromJSON LoggingConfiguration where
  parseJSON =
    Core.withObject
      "LoggingConfiguration"
      ( \x ->
          LoggingConfiguration'
            Prelude.<$> (x Core..:? "LoggingRole")
            Prelude.<*> (x Core..:? "LogGroupName")
      )

instance Prelude.Hashable LoggingConfiguration

instance Prelude.NFData LoggingConfiguration

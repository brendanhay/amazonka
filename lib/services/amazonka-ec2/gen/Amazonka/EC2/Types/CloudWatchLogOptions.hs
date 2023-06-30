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
-- Module      : Amazonka.EC2.Types.CloudWatchLogOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CloudWatchLogOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Options for sending VPN tunnel logs to CloudWatch.
--
-- /See:/ 'newCloudWatchLogOptions' smart constructor.
data CloudWatchLogOptions = CloudWatchLogOptions'
  { -- | Status of VPN tunnel logging feature. Default value is @False@.
    --
    -- Valid values: @True@ | @False@
    logEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the CloudWatch log group to send logs
    -- to.
    logGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Configured log format. Default format is @json@.
    --
    -- Valid values: @json@ | @text@
    logOutputFormat :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchLogOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logEnabled', 'cloudWatchLogOptions_logEnabled' - Status of VPN tunnel logging feature. Default value is @False@.
--
-- Valid values: @True@ | @False@
--
-- 'logGroupArn', 'cloudWatchLogOptions_logGroupArn' - The Amazon Resource Name (ARN) of the CloudWatch log group to send logs
-- to.
--
-- 'logOutputFormat', 'cloudWatchLogOptions_logOutputFormat' - Configured log format. Default format is @json@.
--
-- Valid values: @json@ | @text@
newCloudWatchLogOptions ::
  CloudWatchLogOptions
newCloudWatchLogOptions =
  CloudWatchLogOptions'
    { logEnabled = Prelude.Nothing,
      logGroupArn = Prelude.Nothing,
      logOutputFormat = Prelude.Nothing
    }

-- | Status of VPN tunnel logging feature. Default value is @False@.
--
-- Valid values: @True@ | @False@
cloudWatchLogOptions_logEnabled :: Lens.Lens' CloudWatchLogOptions (Prelude.Maybe Prelude.Bool)
cloudWatchLogOptions_logEnabled = Lens.lens (\CloudWatchLogOptions' {logEnabled} -> logEnabled) (\s@CloudWatchLogOptions' {} a -> s {logEnabled = a} :: CloudWatchLogOptions)

-- | The Amazon Resource Name (ARN) of the CloudWatch log group to send logs
-- to.
cloudWatchLogOptions_logGroupArn :: Lens.Lens' CloudWatchLogOptions (Prelude.Maybe Prelude.Text)
cloudWatchLogOptions_logGroupArn = Lens.lens (\CloudWatchLogOptions' {logGroupArn} -> logGroupArn) (\s@CloudWatchLogOptions' {} a -> s {logGroupArn = a} :: CloudWatchLogOptions)

-- | Configured log format. Default format is @json@.
--
-- Valid values: @json@ | @text@
cloudWatchLogOptions_logOutputFormat :: Lens.Lens' CloudWatchLogOptions (Prelude.Maybe Prelude.Text)
cloudWatchLogOptions_logOutputFormat = Lens.lens (\CloudWatchLogOptions' {logOutputFormat} -> logOutputFormat) (\s@CloudWatchLogOptions' {} a -> s {logOutputFormat = a} :: CloudWatchLogOptions)

instance Data.FromXML CloudWatchLogOptions where
  parseXML x =
    CloudWatchLogOptions'
      Prelude.<$> (x Data..@? "logEnabled")
      Prelude.<*> (x Data..@? "logGroupArn")
      Prelude.<*> (x Data..@? "logOutputFormat")

instance Prelude.Hashable CloudWatchLogOptions where
  hashWithSalt _salt CloudWatchLogOptions' {..} =
    _salt
      `Prelude.hashWithSalt` logEnabled
      `Prelude.hashWithSalt` logGroupArn
      `Prelude.hashWithSalt` logOutputFormat

instance Prelude.NFData CloudWatchLogOptions where
  rnf CloudWatchLogOptions' {..} =
    Prelude.rnf logEnabled
      `Prelude.seq` Prelude.rnf logGroupArn
      `Prelude.seq` Prelude.rnf logOutputFormat

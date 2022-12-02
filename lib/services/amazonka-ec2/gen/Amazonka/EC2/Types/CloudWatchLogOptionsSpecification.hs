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
-- Module      : Amazonka.EC2.Types.CloudWatchLogOptionsSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CloudWatchLogOptionsSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Options for sending VPN tunnel logs to CloudWatch.
--
-- /See:/ 'newCloudWatchLogOptionsSpecification' smart constructor.
data CloudWatchLogOptionsSpecification = CloudWatchLogOptionsSpecification'
  { -- | The Amazon Resource Name (ARN) of the CloudWatch log group to send logs
    -- to.
    logGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Enable or disable VPN tunnel logging feature. Default value is @False@.
    --
    -- Valid values: @True@ | @False@
    logEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Set log format. Default format is @json@.
    --
    -- Valid values: @json@ | @text@
    logOutputFormat :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchLogOptionsSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupArn', 'cloudWatchLogOptionsSpecification_logGroupArn' - The Amazon Resource Name (ARN) of the CloudWatch log group to send logs
-- to.
--
-- 'logEnabled', 'cloudWatchLogOptionsSpecification_logEnabled' - Enable or disable VPN tunnel logging feature. Default value is @False@.
--
-- Valid values: @True@ | @False@
--
-- 'logOutputFormat', 'cloudWatchLogOptionsSpecification_logOutputFormat' - Set log format. Default format is @json@.
--
-- Valid values: @json@ | @text@
newCloudWatchLogOptionsSpecification ::
  CloudWatchLogOptionsSpecification
newCloudWatchLogOptionsSpecification =
  CloudWatchLogOptionsSpecification'
    { logGroupArn =
        Prelude.Nothing,
      logEnabled = Prelude.Nothing,
      logOutputFormat = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the CloudWatch log group to send logs
-- to.
cloudWatchLogOptionsSpecification_logGroupArn :: Lens.Lens' CloudWatchLogOptionsSpecification (Prelude.Maybe Prelude.Text)
cloudWatchLogOptionsSpecification_logGroupArn = Lens.lens (\CloudWatchLogOptionsSpecification' {logGroupArn} -> logGroupArn) (\s@CloudWatchLogOptionsSpecification' {} a -> s {logGroupArn = a} :: CloudWatchLogOptionsSpecification)

-- | Enable or disable VPN tunnel logging feature. Default value is @False@.
--
-- Valid values: @True@ | @False@
cloudWatchLogOptionsSpecification_logEnabled :: Lens.Lens' CloudWatchLogOptionsSpecification (Prelude.Maybe Prelude.Bool)
cloudWatchLogOptionsSpecification_logEnabled = Lens.lens (\CloudWatchLogOptionsSpecification' {logEnabled} -> logEnabled) (\s@CloudWatchLogOptionsSpecification' {} a -> s {logEnabled = a} :: CloudWatchLogOptionsSpecification)

-- | Set log format. Default format is @json@.
--
-- Valid values: @json@ | @text@
cloudWatchLogOptionsSpecification_logOutputFormat :: Lens.Lens' CloudWatchLogOptionsSpecification (Prelude.Maybe Prelude.Text)
cloudWatchLogOptionsSpecification_logOutputFormat = Lens.lens (\CloudWatchLogOptionsSpecification' {logOutputFormat} -> logOutputFormat) (\s@CloudWatchLogOptionsSpecification' {} a -> s {logOutputFormat = a} :: CloudWatchLogOptionsSpecification)

instance
  Prelude.Hashable
    CloudWatchLogOptionsSpecification
  where
  hashWithSalt
    _salt
    CloudWatchLogOptionsSpecification' {..} =
      _salt `Prelude.hashWithSalt` logGroupArn
        `Prelude.hashWithSalt` logEnabled
        `Prelude.hashWithSalt` logOutputFormat

instance
  Prelude.NFData
    CloudWatchLogOptionsSpecification
  where
  rnf CloudWatchLogOptionsSpecification' {..} =
    Prelude.rnf logGroupArn
      `Prelude.seq` Prelude.rnf logEnabled
      `Prelude.seq` Prelude.rnf logOutputFormat

instance
  Data.ToQuery
    CloudWatchLogOptionsSpecification
  where
  toQuery CloudWatchLogOptionsSpecification' {..} =
    Prelude.mconcat
      [ "LogGroupArn" Data.=: logGroupArn,
        "LogEnabled" Data.=: logEnabled,
        "LogOutputFormat" Data.=: logOutputFormat
      ]

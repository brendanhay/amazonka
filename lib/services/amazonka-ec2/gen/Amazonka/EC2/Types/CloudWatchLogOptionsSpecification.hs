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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | Enable or disable VPN tunnel logging feature. Default value is @False@.
    --
    -- Valid values: @True@ | @False@
    logEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the CloudWatch log group to send logs
    -- to.
    logGroupArn :: Prelude.Maybe Prelude.Text,
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
-- 'logEnabled', 'cloudWatchLogOptionsSpecification_logEnabled' - Enable or disable VPN tunnel logging feature. Default value is @False@.
--
-- Valid values: @True@ | @False@
--
-- 'logGroupArn', 'cloudWatchLogOptionsSpecification_logGroupArn' - The Amazon Resource Name (ARN) of the CloudWatch log group to send logs
-- to.
--
-- 'logOutputFormat', 'cloudWatchLogOptionsSpecification_logOutputFormat' - Set log format. Default format is @json@.
--
-- Valid values: @json@ | @text@
newCloudWatchLogOptionsSpecification ::
  CloudWatchLogOptionsSpecification
newCloudWatchLogOptionsSpecification =
  CloudWatchLogOptionsSpecification'
    { logEnabled =
        Prelude.Nothing,
      logGroupArn = Prelude.Nothing,
      logOutputFormat = Prelude.Nothing
    }

-- | Enable or disable VPN tunnel logging feature. Default value is @False@.
--
-- Valid values: @True@ | @False@
cloudWatchLogOptionsSpecification_logEnabled :: Lens.Lens' CloudWatchLogOptionsSpecification (Prelude.Maybe Prelude.Bool)
cloudWatchLogOptionsSpecification_logEnabled = Lens.lens (\CloudWatchLogOptionsSpecification' {logEnabled} -> logEnabled) (\s@CloudWatchLogOptionsSpecification' {} a -> s {logEnabled = a} :: CloudWatchLogOptionsSpecification)

-- | The Amazon Resource Name (ARN) of the CloudWatch log group to send logs
-- to.
cloudWatchLogOptionsSpecification_logGroupArn :: Lens.Lens' CloudWatchLogOptionsSpecification (Prelude.Maybe Prelude.Text)
cloudWatchLogOptionsSpecification_logGroupArn = Lens.lens (\CloudWatchLogOptionsSpecification' {logGroupArn} -> logGroupArn) (\s@CloudWatchLogOptionsSpecification' {} a -> s {logGroupArn = a} :: CloudWatchLogOptionsSpecification)

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
      _salt
        `Prelude.hashWithSalt` logEnabled
        `Prelude.hashWithSalt` logGroupArn
        `Prelude.hashWithSalt` logOutputFormat

instance
  Prelude.NFData
    CloudWatchLogOptionsSpecification
  where
  rnf CloudWatchLogOptionsSpecification' {..} =
    Prelude.rnf logEnabled `Prelude.seq`
      Prelude.rnf logGroupArn `Prelude.seq`
        Prelude.rnf logOutputFormat

instance
  Data.ToQuery
    CloudWatchLogOptionsSpecification
  where
  toQuery CloudWatchLogOptionsSpecification' {..} =
    Prelude.mconcat
      [ "LogEnabled" Data.=: logEnabled,
        "LogGroupArn" Data.=: logGroupArn,
        "LogOutputFormat" Data.=: logOutputFormat
      ]

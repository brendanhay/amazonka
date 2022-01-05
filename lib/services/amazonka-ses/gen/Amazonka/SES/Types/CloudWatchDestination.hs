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
-- Module      : Amazonka.SES.Types.CloudWatchDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.CloudWatchDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SES.Types.CloudWatchDimensionConfiguration

-- | Contains information associated with an Amazon CloudWatch event
-- destination to which email sending events are published.
--
-- Event destinations, such as Amazon CloudWatch, are associated with
-- configuration sets, which enable you to publish email sending events.
-- For information about using configuration sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- /See:/ 'newCloudWatchDestination' smart constructor.
data CloudWatchDestination = CloudWatchDestination'
  { -- | A list of dimensions upon which to categorize your emails when you
    -- publish email sending events to Amazon CloudWatch.
    dimensionConfigurations :: [CloudWatchDimensionConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensionConfigurations', 'cloudWatchDestination_dimensionConfigurations' - A list of dimensions upon which to categorize your emails when you
-- publish email sending events to Amazon CloudWatch.
newCloudWatchDestination ::
  CloudWatchDestination
newCloudWatchDestination =
  CloudWatchDestination'
    { dimensionConfigurations =
        Prelude.mempty
    }

-- | A list of dimensions upon which to categorize your emails when you
-- publish email sending events to Amazon CloudWatch.
cloudWatchDestination_dimensionConfigurations :: Lens.Lens' CloudWatchDestination [CloudWatchDimensionConfiguration]
cloudWatchDestination_dimensionConfigurations = Lens.lens (\CloudWatchDestination' {dimensionConfigurations} -> dimensionConfigurations) (\s@CloudWatchDestination' {} a -> s {dimensionConfigurations = a} :: CloudWatchDestination) Prelude.. Lens.coerced

instance Core.FromXML CloudWatchDestination where
  parseXML x =
    CloudWatchDestination'
      Prelude.<$> ( x Core..@? "DimensionConfigurations"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.parseXMLList "member"
                  )

instance Prelude.Hashable CloudWatchDestination where
  hashWithSalt _salt CloudWatchDestination' {..} =
    _salt
      `Prelude.hashWithSalt` dimensionConfigurations

instance Prelude.NFData CloudWatchDestination where
  rnf CloudWatchDestination' {..} =
    Prelude.rnf dimensionConfigurations

instance Core.ToQuery CloudWatchDestination where
  toQuery CloudWatchDestination' {..} =
    Prelude.mconcat
      [ "DimensionConfigurations"
          Core.=: Core.toQueryList "member" dimensionConfigurations
      ]

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
-- Module      : Network.AWS.SES.Types.CloudWatchDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.CloudWatchDestination where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SES.Types.CloudWatchDimensionConfiguration

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
cloudWatchDestination_dimensionConfigurations = Lens.lens (\CloudWatchDestination' {dimensionConfigurations} -> dimensionConfigurations) (\s@CloudWatchDestination' {} a -> s {dimensionConfigurations = a} :: CloudWatchDestination) Prelude.. Prelude._Coerce

instance Prelude.FromXML CloudWatchDestination where
  parseXML x =
    CloudWatchDestination'
      Prelude.<$> ( x Prelude..@? "DimensionConfigurations"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.parseXMLList "member"
                  )

instance Prelude.Hashable CloudWatchDestination

instance Prelude.NFData CloudWatchDestination

instance Prelude.ToQuery CloudWatchDestination where
  toQuery CloudWatchDestination' {..} =
    Prelude.mconcat
      [ "DimensionConfigurations"
          Prelude.=: Prelude.toQueryList "member" dimensionConfigurations
      ]

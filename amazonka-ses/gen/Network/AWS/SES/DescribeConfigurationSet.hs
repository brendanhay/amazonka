{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DescribeConfigurationSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of the specified configuration set. For information
-- about using configuration sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DescribeConfigurationSet
  ( -- * Creating a Request
    DescribeConfigurationSet (..),
    newDescribeConfigurationSet,

    -- * Request Lenses
    describeConfigurationSet_configurationSetAttributeNames,
    describeConfigurationSet_configurationSetName,

    -- * Destructuring the Response
    DescribeConfigurationSetResponse (..),
    newDescribeConfigurationSetResponse,

    -- * Response Lenses
    describeConfigurationSetResponse_trackingOptions,
    describeConfigurationSetResponse_deliveryOptions,
    describeConfigurationSetResponse_reputationOptions,
    describeConfigurationSetResponse_eventDestinations,
    describeConfigurationSetResponse_configurationSet,
    describeConfigurationSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to return the details of a configuration set.
-- Configuration sets enable you to publish email sending events. For
-- information about using configuration sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- /See:/ 'newDescribeConfigurationSet' smart constructor.
data DescribeConfigurationSet = DescribeConfigurationSet'
  { -- | A list of configuration set attributes to return.
    configurationSetAttributeNames :: Core.Maybe [ConfigurationSetAttribute],
    -- | The name of the configuration set to describe.
    configurationSetName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeConfigurationSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetAttributeNames', 'describeConfigurationSet_configurationSetAttributeNames' - A list of configuration set attributes to return.
--
-- 'configurationSetName', 'describeConfigurationSet_configurationSetName' - The name of the configuration set to describe.
newDescribeConfigurationSet ::
  -- | 'configurationSetName'
  Core.Text ->
  DescribeConfigurationSet
newDescribeConfigurationSet pConfigurationSetName_ =
  DescribeConfigurationSet'
    { configurationSetAttributeNames =
        Core.Nothing,
      configurationSetName = pConfigurationSetName_
    }

-- | A list of configuration set attributes to return.
describeConfigurationSet_configurationSetAttributeNames :: Lens.Lens' DescribeConfigurationSet (Core.Maybe [ConfigurationSetAttribute])
describeConfigurationSet_configurationSetAttributeNames = Lens.lens (\DescribeConfigurationSet' {configurationSetAttributeNames} -> configurationSetAttributeNames) (\s@DescribeConfigurationSet' {} a -> s {configurationSetAttributeNames = a} :: DescribeConfigurationSet) Core.. Lens.mapping Lens._Coerce

-- | The name of the configuration set to describe.
describeConfigurationSet_configurationSetName :: Lens.Lens' DescribeConfigurationSet Core.Text
describeConfigurationSet_configurationSetName = Lens.lens (\DescribeConfigurationSet' {configurationSetName} -> configurationSetName) (\s@DescribeConfigurationSet' {} a -> s {configurationSetName = a} :: DescribeConfigurationSet)

instance Core.AWSRequest DescribeConfigurationSet where
  type
    AWSResponse DescribeConfigurationSet =
      DescribeConfigurationSetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeConfigurationSetResult"
      ( \s h x ->
          DescribeConfigurationSetResponse'
            Core.<$> (x Core..@? "TrackingOptions")
            Core.<*> (x Core..@? "DeliveryOptions")
            Core.<*> (x Core..@? "ReputationOptions")
            Core.<*> ( x Core..@? "EventDestinations" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "ConfigurationSet")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeConfigurationSet

instance Core.NFData DescribeConfigurationSet

instance Core.ToHeaders DescribeConfigurationSet where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeConfigurationSet where
  toPath = Core.const "/"

instance Core.ToQuery DescribeConfigurationSet where
  toQuery DescribeConfigurationSet' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeConfigurationSet" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "ConfigurationSetAttributeNames"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> configurationSetAttributeNames
            ),
        "ConfigurationSetName" Core.=: configurationSetName
      ]

-- | Represents the details of a configuration set. Configuration sets enable
-- you to publish email sending events. For information about using
-- configuration sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- /See:/ 'newDescribeConfigurationSetResponse' smart constructor.
data DescribeConfigurationSetResponse = DescribeConfigurationSetResponse'
  { -- | The name of the custom open and click tracking domain associated with
    -- the configuration set.
    trackingOptions :: Core.Maybe TrackingOptions,
    deliveryOptions :: Core.Maybe DeliveryOptions,
    -- | An object that represents the reputation settings for the configuration
    -- set.
    reputationOptions :: Core.Maybe ReputationOptions,
    -- | A list of event destinations associated with the configuration set.
    eventDestinations :: Core.Maybe [EventDestination],
    -- | The configuration set object associated with the specified configuration
    -- set.
    configurationSet :: Core.Maybe ConfigurationSet,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeConfigurationSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trackingOptions', 'describeConfigurationSetResponse_trackingOptions' - The name of the custom open and click tracking domain associated with
-- the configuration set.
--
-- 'deliveryOptions', 'describeConfigurationSetResponse_deliveryOptions' - Undocumented member.
--
-- 'reputationOptions', 'describeConfigurationSetResponse_reputationOptions' - An object that represents the reputation settings for the configuration
-- set.
--
-- 'eventDestinations', 'describeConfigurationSetResponse_eventDestinations' - A list of event destinations associated with the configuration set.
--
-- 'configurationSet', 'describeConfigurationSetResponse_configurationSet' - The configuration set object associated with the specified configuration
-- set.
--
-- 'httpStatus', 'describeConfigurationSetResponse_httpStatus' - The response's http status code.
newDescribeConfigurationSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeConfigurationSetResponse
newDescribeConfigurationSetResponse pHttpStatus_ =
  DescribeConfigurationSetResponse'
    { trackingOptions =
        Core.Nothing,
      deliveryOptions = Core.Nothing,
      reputationOptions = Core.Nothing,
      eventDestinations = Core.Nothing,
      configurationSet = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the custom open and click tracking domain associated with
-- the configuration set.
describeConfigurationSetResponse_trackingOptions :: Lens.Lens' DescribeConfigurationSetResponse (Core.Maybe TrackingOptions)
describeConfigurationSetResponse_trackingOptions = Lens.lens (\DescribeConfigurationSetResponse' {trackingOptions} -> trackingOptions) (\s@DescribeConfigurationSetResponse' {} a -> s {trackingOptions = a} :: DescribeConfigurationSetResponse)

-- | Undocumented member.
describeConfigurationSetResponse_deliveryOptions :: Lens.Lens' DescribeConfigurationSetResponse (Core.Maybe DeliveryOptions)
describeConfigurationSetResponse_deliveryOptions = Lens.lens (\DescribeConfigurationSetResponse' {deliveryOptions} -> deliveryOptions) (\s@DescribeConfigurationSetResponse' {} a -> s {deliveryOptions = a} :: DescribeConfigurationSetResponse)

-- | An object that represents the reputation settings for the configuration
-- set.
describeConfigurationSetResponse_reputationOptions :: Lens.Lens' DescribeConfigurationSetResponse (Core.Maybe ReputationOptions)
describeConfigurationSetResponse_reputationOptions = Lens.lens (\DescribeConfigurationSetResponse' {reputationOptions} -> reputationOptions) (\s@DescribeConfigurationSetResponse' {} a -> s {reputationOptions = a} :: DescribeConfigurationSetResponse)

-- | A list of event destinations associated with the configuration set.
describeConfigurationSetResponse_eventDestinations :: Lens.Lens' DescribeConfigurationSetResponse (Core.Maybe [EventDestination])
describeConfigurationSetResponse_eventDestinations = Lens.lens (\DescribeConfigurationSetResponse' {eventDestinations} -> eventDestinations) (\s@DescribeConfigurationSetResponse' {} a -> s {eventDestinations = a} :: DescribeConfigurationSetResponse) Core.. Lens.mapping Lens._Coerce

-- | The configuration set object associated with the specified configuration
-- set.
describeConfigurationSetResponse_configurationSet :: Lens.Lens' DescribeConfigurationSetResponse (Core.Maybe ConfigurationSet)
describeConfigurationSetResponse_configurationSet = Lens.lens (\DescribeConfigurationSetResponse' {configurationSet} -> configurationSet) (\s@DescribeConfigurationSetResponse' {} a -> s {configurationSet = a} :: DescribeConfigurationSetResponse)

-- | The response's http status code.
describeConfigurationSetResponse_httpStatus :: Lens.Lens' DescribeConfigurationSetResponse Core.Int
describeConfigurationSetResponse_httpStatus = Lens.lens (\DescribeConfigurationSetResponse' {httpStatus} -> httpStatus) (\s@DescribeConfigurationSetResponse' {} a -> s {httpStatus = a} :: DescribeConfigurationSetResponse)

instance Core.NFData DescribeConfigurationSetResponse

{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    configurationSetAttributeNames :: Prelude.Maybe [ConfigurationSetAttribute],
    -- | The name of the configuration set to describe.
    configurationSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DescribeConfigurationSet
newDescribeConfigurationSet pConfigurationSetName_ =
  DescribeConfigurationSet'
    { configurationSetAttributeNames =
        Prelude.Nothing,
      configurationSetName = pConfigurationSetName_
    }

-- | A list of configuration set attributes to return.
describeConfigurationSet_configurationSetAttributeNames :: Lens.Lens' DescribeConfigurationSet (Prelude.Maybe [ConfigurationSetAttribute])
describeConfigurationSet_configurationSetAttributeNames = Lens.lens (\DescribeConfigurationSet' {configurationSetAttributeNames} -> configurationSetAttributeNames) (\s@DescribeConfigurationSet' {} a -> s {configurationSetAttributeNames = a} :: DescribeConfigurationSet) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the configuration set to describe.
describeConfigurationSet_configurationSetName :: Lens.Lens' DescribeConfigurationSet Prelude.Text
describeConfigurationSet_configurationSetName = Lens.lens (\DescribeConfigurationSet' {configurationSetName} -> configurationSetName) (\s@DescribeConfigurationSet' {} a -> s {configurationSetName = a} :: DescribeConfigurationSet)

instance Prelude.AWSRequest DescribeConfigurationSet where
  type
    Rs DescribeConfigurationSet =
      DescribeConfigurationSetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeConfigurationSetResult"
      ( \s h x ->
          DescribeConfigurationSetResponse'
            Prelude.<$> (x Prelude..@? "TrackingOptions")
            Prelude.<*> (x Prelude..@? "DeliveryOptions")
            Prelude.<*> (x Prelude..@? "ReputationOptions")
            Prelude.<*> ( x Prelude..@? "EventDestinations"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (x Prelude..@? "ConfigurationSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeConfigurationSet

instance Prelude.NFData DescribeConfigurationSet

instance Prelude.ToHeaders DescribeConfigurationSet where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeConfigurationSet where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeConfigurationSet where
  toQuery DescribeConfigurationSet' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeConfigurationSet" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "ConfigurationSetAttributeNames"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> configurationSetAttributeNames
            ),
        "ConfigurationSetName"
          Prelude.=: configurationSetName
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
    trackingOptions :: Prelude.Maybe TrackingOptions,
    deliveryOptions :: Prelude.Maybe DeliveryOptions,
    -- | An object that represents the reputation settings for the configuration
    -- set.
    reputationOptions :: Prelude.Maybe ReputationOptions,
    -- | A list of event destinations associated with the configuration set.
    eventDestinations :: Prelude.Maybe [EventDestination],
    -- | The configuration set object associated with the specified configuration
    -- set.
    configurationSet :: Prelude.Maybe ConfigurationSet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeConfigurationSetResponse
newDescribeConfigurationSetResponse pHttpStatus_ =
  DescribeConfigurationSetResponse'
    { trackingOptions =
        Prelude.Nothing,
      deliveryOptions = Prelude.Nothing,
      reputationOptions = Prelude.Nothing,
      eventDestinations = Prelude.Nothing,
      configurationSet = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the custom open and click tracking domain associated with
-- the configuration set.
describeConfigurationSetResponse_trackingOptions :: Lens.Lens' DescribeConfigurationSetResponse (Prelude.Maybe TrackingOptions)
describeConfigurationSetResponse_trackingOptions = Lens.lens (\DescribeConfigurationSetResponse' {trackingOptions} -> trackingOptions) (\s@DescribeConfigurationSetResponse' {} a -> s {trackingOptions = a} :: DescribeConfigurationSetResponse)

-- | Undocumented member.
describeConfigurationSetResponse_deliveryOptions :: Lens.Lens' DescribeConfigurationSetResponse (Prelude.Maybe DeliveryOptions)
describeConfigurationSetResponse_deliveryOptions = Lens.lens (\DescribeConfigurationSetResponse' {deliveryOptions} -> deliveryOptions) (\s@DescribeConfigurationSetResponse' {} a -> s {deliveryOptions = a} :: DescribeConfigurationSetResponse)

-- | An object that represents the reputation settings for the configuration
-- set.
describeConfigurationSetResponse_reputationOptions :: Lens.Lens' DescribeConfigurationSetResponse (Prelude.Maybe ReputationOptions)
describeConfigurationSetResponse_reputationOptions = Lens.lens (\DescribeConfigurationSetResponse' {reputationOptions} -> reputationOptions) (\s@DescribeConfigurationSetResponse' {} a -> s {reputationOptions = a} :: DescribeConfigurationSetResponse)

-- | A list of event destinations associated with the configuration set.
describeConfigurationSetResponse_eventDestinations :: Lens.Lens' DescribeConfigurationSetResponse (Prelude.Maybe [EventDestination])
describeConfigurationSetResponse_eventDestinations = Lens.lens (\DescribeConfigurationSetResponse' {eventDestinations} -> eventDestinations) (\s@DescribeConfigurationSetResponse' {} a -> s {eventDestinations = a} :: DescribeConfigurationSetResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The configuration set object associated with the specified configuration
-- set.
describeConfigurationSetResponse_configurationSet :: Lens.Lens' DescribeConfigurationSetResponse (Prelude.Maybe ConfigurationSet)
describeConfigurationSetResponse_configurationSet = Lens.lens (\DescribeConfigurationSetResponse' {configurationSet} -> configurationSet) (\s@DescribeConfigurationSetResponse' {} a -> s {configurationSet = a} :: DescribeConfigurationSetResponse)

-- | The response's http status code.
describeConfigurationSetResponse_httpStatus :: Lens.Lens' DescribeConfigurationSetResponse Prelude.Int
describeConfigurationSetResponse_httpStatus = Lens.lens (\DescribeConfigurationSetResponse' {httpStatus} -> httpStatus) (\s@DescribeConfigurationSetResponse' {} a -> s {httpStatus = a} :: DescribeConfigurationSetResponse)

instance
  Prelude.NFData
    DescribeConfigurationSetResponse

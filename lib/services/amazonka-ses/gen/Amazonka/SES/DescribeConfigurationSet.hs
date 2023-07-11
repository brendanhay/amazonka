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
-- Module      : Amazonka.SES.DescribeConfigurationSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of the specified configuration set. For information
-- about using configuration sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.DescribeConfigurationSet
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
    describeConfigurationSetResponse_configurationSet,
    describeConfigurationSetResponse_deliveryOptions,
    describeConfigurationSetResponse_eventDestinations,
    describeConfigurationSetResponse_reputationOptions,
    describeConfigurationSetResponse_trackingOptions,
    describeConfigurationSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
describeConfigurationSet_configurationSetAttributeNames = Lens.lens (\DescribeConfigurationSet' {configurationSetAttributeNames} -> configurationSetAttributeNames) (\s@DescribeConfigurationSet' {} a -> s {configurationSetAttributeNames = a} :: DescribeConfigurationSet) Prelude.. Lens.mapping Lens.coerced

-- | The name of the configuration set to describe.
describeConfigurationSet_configurationSetName :: Lens.Lens' DescribeConfigurationSet Prelude.Text
describeConfigurationSet_configurationSetName = Lens.lens (\DescribeConfigurationSet' {configurationSetName} -> configurationSetName) (\s@DescribeConfigurationSet' {} a -> s {configurationSetName = a} :: DescribeConfigurationSet)

instance Core.AWSRequest DescribeConfigurationSet where
  type
    AWSResponse DescribeConfigurationSet =
      DescribeConfigurationSetResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeConfigurationSetResult"
      ( \s h x ->
          DescribeConfigurationSetResponse'
            Prelude.<$> (x Data..@? "ConfigurationSet")
            Prelude.<*> (x Data..@? "DeliveryOptions")
            Prelude.<*> ( x
                            Data..@? "EventDestinations"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "ReputationOptions")
            Prelude.<*> (x Data..@? "TrackingOptions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeConfigurationSet where
  hashWithSalt _salt DescribeConfigurationSet' {..} =
    _salt
      `Prelude.hashWithSalt` configurationSetAttributeNames
      `Prelude.hashWithSalt` configurationSetName

instance Prelude.NFData DescribeConfigurationSet where
  rnf DescribeConfigurationSet' {..} =
    Prelude.rnf configurationSetAttributeNames
      `Prelude.seq` Prelude.rnf configurationSetName

instance Data.ToHeaders DescribeConfigurationSet where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeConfigurationSet where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeConfigurationSet where
  toQuery DescribeConfigurationSet' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeConfigurationSet" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "ConfigurationSetAttributeNames"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> configurationSetAttributeNames
            ),
        "ConfigurationSetName" Data.=: configurationSetName
      ]

-- | Represents the details of a configuration set. Configuration sets enable
-- you to publish email sending events. For information about using
-- configuration sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- /See:/ 'newDescribeConfigurationSetResponse' smart constructor.
data DescribeConfigurationSetResponse = DescribeConfigurationSetResponse'
  { -- | The configuration set object associated with the specified configuration
    -- set.
    configurationSet :: Prelude.Maybe ConfigurationSet,
    deliveryOptions :: Prelude.Maybe DeliveryOptions,
    -- | A list of event destinations associated with the configuration set.
    eventDestinations :: Prelude.Maybe [EventDestination],
    -- | An object that represents the reputation settings for the configuration
    -- set.
    reputationOptions :: Prelude.Maybe ReputationOptions,
    -- | The name of the custom open and click tracking domain associated with
    -- the configuration set.
    trackingOptions :: Prelude.Maybe TrackingOptions,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConfigurationSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSet', 'describeConfigurationSetResponse_configurationSet' - The configuration set object associated with the specified configuration
-- set.
--
-- 'deliveryOptions', 'describeConfigurationSetResponse_deliveryOptions' - Undocumented member.
--
-- 'eventDestinations', 'describeConfigurationSetResponse_eventDestinations' - A list of event destinations associated with the configuration set.
--
-- 'reputationOptions', 'describeConfigurationSetResponse_reputationOptions' - An object that represents the reputation settings for the configuration
-- set.
--
-- 'trackingOptions', 'describeConfigurationSetResponse_trackingOptions' - The name of the custom open and click tracking domain associated with
-- the configuration set.
--
-- 'httpStatus', 'describeConfigurationSetResponse_httpStatus' - The response's http status code.
newDescribeConfigurationSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConfigurationSetResponse
newDescribeConfigurationSetResponse pHttpStatus_ =
  DescribeConfigurationSetResponse'
    { configurationSet =
        Prelude.Nothing,
      deliveryOptions = Prelude.Nothing,
      eventDestinations = Prelude.Nothing,
      reputationOptions = Prelude.Nothing,
      trackingOptions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The configuration set object associated with the specified configuration
-- set.
describeConfigurationSetResponse_configurationSet :: Lens.Lens' DescribeConfigurationSetResponse (Prelude.Maybe ConfigurationSet)
describeConfigurationSetResponse_configurationSet = Lens.lens (\DescribeConfigurationSetResponse' {configurationSet} -> configurationSet) (\s@DescribeConfigurationSetResponse' {} a -> s {configurationSet = a} :: DescribeConfigurationSetResponse)

-- | Undocumented member.
describeConfigurationSetResponse_deliveryOptions :: Lens.Lens' DescribeConfigurationSetResponse (Prelude.Maybe DeliveryOptions)
describeConfigurationSetResponse_deliveryOptions = Lens.lens (\DescribeConfigurationSetResponse' {deliveryOptions} -> deliveryOptions) (\s@DescribeConfigurationSetResponse' {} a -> s {deliveryOptions = a} :: DescribeConfigurationSetResponse)

-- | A list of event destinations associated with the configuration set.
describeConfigurationSetResponse_eventDestinations :: Lens.Lens' DescribeConfigurationSetResponse (Prelude.Maybe [EventDestination])
describeConfigurationSetResponse_eventDestinations = Lens.lens (\DescribeConfigurationSetResponse' {eventDestinations} -> eventDestinations) (\s@DescribeConfigurationSetResponse' {} a -> s {eventDestinations = a} :: DescribeConfigurationSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | An object that represents the reputation settings for the configuration
-- set.
describeConfigurationSetResponse_reputationOptions :: Lens.Lens' DescribeConfigurationSetResponse (Prelude.Maybe ReputationOptions)
describeConfigurationSetResponse_reputationOptions = Lens.lens (\DescribeConfigurationSetResponse' {reputationOptions} -> reputationOptions) (\s@DescribeConfigurationSetResponse' {} a -> s {reputationOptions = a} :: DescribeConfigurationSetResponse)

-- | The name of the custom open and click tracking domain associated with
-- the configuration set.
describeConfigurationSetResponse_trackingOptions :: Lens.Lens' DescribeConfigurationSetResponse (Prelude.Maybe TrackingOptions)
describeConfigurationSetResponse_trackingOptions = Lens.lens (\DescribeConfigurationSetResponse' {trackingOptions} -> trackingOptions) (\s@DescribeConfigurationSetResponse' {} a -> s {trackingOptions = a} :: DescribeConfigurationSetResponse)

-- | The response's http status code.
describeConfigurationSetResponse_httpStatus :: Lens.Lens' DescribeConfigurationSetResponse Prelude.Int
describeConfigurationSetResponse_httpStatus = Lens.lens (\DescribeConfigurationSetResponse' {httpStatus} -> httpStatus) (\s@DescribeConfigurationSetResponse' {} a -> s {httpStatus = a} :: DescribeConfigurationSetResponse)

instance
  Prelude.NFData
    DescribeConfigurationSetResponse
  where
  rnf DescribeConfigurationSetResponse' {..} =
    Prelude.rnf configurationSet
      `Prelude.seq` Prelude.rnf deliveryOptions
      `Prelude.seq` Prelude.rnf eventDestinations
      `Prelude.seq` Prelude.rnf reputationOptions
      `Prelude.seq` Prelude.rnf trackingOptions
      `Prelude.seq` Prelude.rnf httpStatus
